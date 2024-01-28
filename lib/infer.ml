open Syntax
module Table = Hashtbl.Make (Int)

type index = int

type cell = typevar option ref
and reference = Ref of cell
and typevar = reference Types.t

type c = Eq of typevar * typevar

type t =
  { stack : cell Stack.t
  ; workq : c Queue.t
  }

let create () =
  let stack = Stack.create () in
  let workq = Queue.create () in
  { stack; workq }
;;

let cell { stack; _ } =
  let r = ref None in
  Stack.push r stack;
  Types.Void (Ref r)
;;

let constraints lang =
  let ct = create () in
  let rec unify lhs rhs =
    match lhs, rhs with
    | Types.Bool, Types.Bool -> ()
    | ( Types.Fn { _from = lfrom; _to = lto }
      , Types.Fn { _from = rfrom; _to = rto } ) ->
      unify lfrom rfrom;
      unify lto rto
    | _ -> Queue.add (Eq (lhs, rhs)) ct.workq
  in
  let tbl = Table.create 1 in
  let rec loop = function
    | Bind v ->
      (match Table.find_opt tbl v with
       | Some t -> t
       | None -> cell ct)
    | Let { _let; _be; _in } ->
      Table.add tbl _let (loop _be);
      loop _in
    | Fn { _from; _to } ->
      let ty_from = cell ct in
      Table.add tbl _from ty_from;
      Types.Fn { _from = ty_from; _to = loop _to }
    | Call { _fn; _with } ->
      let ty_ret = cell ct in
      unify (loop _fn) (Types.Fn { _from = loop _with; _to = ty_ret });
      ty_ret
    | If { _if; _then; _else } ->
      unify (loop _if) Types.Bool;
      let ty_then = loop _then in
      unify ty_then (loop _else);
      ty_then
    | Bool _b -> Types.Bool
  in
  loop lang, tbl, ct
;;

exception TypeError

let to_type tbl =
  let rec concrete : reference Types.t -> unit Types.t = function
    | Types.Void (Ref r) ->
      (match !r with
       | Some t -> concrete t
       | None -> raise TypeError)
    | Types.Bool -> Types.Bool
    | Types.Fn { _from; _to } ->
      Types.Fn { _from = concrete _from; _to = concrete _to }
  in
  let types = Table.create (Table.length tbl) in
  Table.iter (fun k v -> Table.add types k (concrete v)) tbl;
  types
;;

let infer lang =
  let _ret, tbl, ct = constraints lang in
  let rec loop = function
    | retries when retries > 0 ->
      let t = Queue.take ct.workq in
      let (Eq (lhs, rhs)) = t in
      let (), less =
        match lhs, rhs with
        | Types.Void (Ref r1), Types.Void (Ref r2) ->
          (match r1, r2 with
           | { contents = Some t }, ({ contents = None } as r)
           | ({ contents = None } as r), { contents = Some t } ->
             (r := Some t), 1
           | { contents = Some t1 }, { contents = Some t2 } ->
             Queue.add (Eq (t1, t2)) ct.workq, 0
           | _, _ -> Queue.add t ct.workq, 1)
        | Types.Void (Ref r), c | c, Types.Void (Ref r) -> (r := Some c), 1
        | _, _ -> Queue.add t ct.workq, 0
      in
      loop (retries - less)
    | _ -> ()
  in
  loop (Queue.length ct.workq);
  if Queue.is_empty ct.workq then Ok (to_type tbl) else Error (tbl, ct)
;;

let%test "true" =
  let _true =
    let ssa_0 = Fn { _from = 1; _to = Bind 1 } in
    let ssa_1 = Call { _fn = Bind 0; _with = Bool true } in
    Let { _let = 0; _be = ssa_0; _in = ssa_1 }
  in
  match infer _true with
  | Error _e -> false
  | Ok t ->
    List.of_seq (Table.to_seq t)
    = [ 0, Types.Fn { _from = Types.Bool; _to = Types.Bool }; 1, Types.Bool ]
;;

let%test "or" =
  let _or =
    let lhs, rhs = Bind 0, Bind 1 in
    let ssa_0 = If { _if = lhs; _then = rhs; _else = lhs } in
    let ssa_1 = Fn { _from = 1; _to = ssa_0 } in
    Fn { _from = 0; _to = ssa_1 }
  in
  match infer _or with
  | Error _e -> false
  | Ok t -> List.of_seq (Table.to_seq t) = [ 0, Types.Bool; 1, Types.Bool ]
;;
