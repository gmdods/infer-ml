open Syntax
module Table = Hashtbl.Make (Int)

type index = int

type cell =
  { mutable cell : typevar option
  ; index : index
  }

and typevar = cell Types.t

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
  let r = { cell = None; index = Stack.length stack } in
  Stack.push r stack;
  Types.Void r
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
  let rec concrete : cell Types.t -> unit Types.t = function
    | Types.Void { cell = None; _ } -> raise TypeError
    | Types.Void { cell = Some t; _ } -> concrete t
    | Types.Bool -> Types.Bool
    | Types.Fn { _from; _to } ->
      Types.Fn { _from = concrete _from; _to = concrete _to }
  in
  let types = Table.create (Table.length tbl) in
  Table.iter (fun k v -> Table.add types k (concrete v)) tbl;
  types
;;

let solver workq = function
  | Eq (Types.Void r1, Types.Void r2) as t ->
    (match r1, r2 with
     | { cell = Some e; _ }, ({ cell = None; _ } as r)
     | ({ cell = None; _ } as r), { cell = Some e; _ } ->
       r.cell <- Some e;
       1
     | { cell = Some t1; _ }, { cell = Some t2; _ } ->
       Queue.add (Eq (t1, t2)) workq;
       0
     | _, _ ->
       Queue.add t workq;
       1)
  | Eq (Types.Void r, c) | Eq (c, Types.Void r) ->
    r.cell <- Some c;
    1
  | t ->
    Queue.add t workq;
    0
;;

let infer lang =
  let _ret, tbl, { workq; _ } = constraints lang in
  let rec loop = function
    | retries when retries > 0 ->
      let less = solver workq (Queue.take workq) in
      loop (retries - less)
    | _ -> ()
  in
  loop (Queue.length workq);
  if Queue.is_empty workq then Ok (to_type tbl) else Error (tbl, workq)
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

let%test "why" =
  let _why =
    let ssa_0 = Call { _fn = Bind 0; _with = Bind 0 } in
    let ssa_1 = Fn { _from = 0; _to = ssa_0 } in
    Call { _fn = ssa_1; _with = Bool true }
  in
  match infer _why with
  | Ok _t -> false
  | Error (_, workq) -> not (Queue.is_empty workq)
;;
