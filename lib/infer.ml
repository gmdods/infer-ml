open Syntax
module Table = Hashtbl.Make (Int)

type index = int

type cell =
  | Ctor of cell Types.t
  | Ref of cell Types.t option ref * index

type fix = Fix of fix Types.t

type c =
  | Eq of
      { lhs : cell
      ; rhs : cell
      }

type t =
  { stack : cell Types.t option ref Stack.t
  ; workq : c Queue.t
  }

let create () =
  let stack = Stack.create () in
  let workq = Queue.create () in
  { stack; workq }
;;

let cell { stack; _ } =
  let index = Stack.length stack in
  let r = ref None in
  Stack.push r stack;
  Ref (r, index)
;;

let rec destructure lc rc =
  match lc, rc with
  | Ctor Types.Bool, Ctor Types.Bool -> []
  | ( Ctor (Types.Fn { _from = lfrom; _to = lto })
    , Ctor (Types.Fn { _from = rfrom; _to = rto }) ) ->
    destructure lfrom rfrom @ destructure lto rto
  | _, _ -> [ lc, rc ]
;;

let constraints lang =
  let ct = create () in
  let add_eq (lhs, rhs) = Queue.add (Eq { lhs; rhs }) ct.workq in
  let unify lhs rhs = destructure lhs rhs |> List.iter add_eq in
  let tbl = Table.create 1 in
  let rec loop = function
    | Bind v ->
      (match Table.find_opt tbl v with
       | Some t -> t
       | None -> cell ct)
    | Let { _let; _be; _in } ->
      let ty = loop _be in
      Table.add tbl _let ty;
      loop _in
    | Fn { _from; _to } ->
      let ty_from = cell ct in
      Table.add tbl _from ty_from;
      let ty_to = loop _to in
      Ctor (Types.Fn { _from = ty_from; _to = ty_to })
    | Call { _fn; _with } ->
      let ty_with = loop _with in
      let ty_ret = cell ct in
      let ty_fn = loop _fn in
      unify ty_fn (Ctor (Types.Fn { _from = ty_with; _to = ty_ret }));
      ty_ret
    | If { _if; _then; _else } ->
      let ty_if = loop _if in
      unify ty_if (Ctor Types.Bool);
      let ty_then = loop _then in
      let ty_else = loop _else in
      unify ty_then ty_else;
      ty_else
    | Bool _b -> Ctor Types.Bool
  in
  loop lang, tbl, ct
;;

exception TypeError

let rec concrete = function
  | Ctor ctor -> restruct ctor
  | Ref (r, _) ->
    (match !r with
     | Some t -> restruct t
     | None -> raise TypeError)

and restruct = function
  | Types.Bool -> Fix Types.Bool
  | Types.Fn { _from; _to } ->
    Fix (Types.Fn { _from = concrete _from; _to = concrete _to })
;;

let infer lang =
  let _ret, tbl, ct = constraints lang in
  let rec loop = function
    | retries when retries > 0 ->
      let t = Queue.take ct.workq in
      let (Eq { lhs; rhs }) = t in
      (match lhs, rhs with
       | Ref (r, _), Ctor c | Ctor c, Ref (r, _) ->
         r := Some c;
         loop (retries - 1)
       | Ref ({ contents = Some t1 }, _), Ref (({ contents = None } as r2), _)
         ->
         r2 := Some t1;
         loop (retries - 1)
       | Ref (({ contents = None } as r1), _), Ref ({ contents = Some t2 }, _)
         ->
         r1 := Some t2;
         loop (retries - 1)
       | Ref ({ contents = Some t1 }, _), Ref ({ contents = Some t2 }, _) ->
         Queue.add (Eq { lhs = Ctor t1; rhs = Ctor t2 }) ct.workq;
         loop retries
       | _ ->
         Queue.add t ct.workq;
         loop (retries - 1))
    | _ -> ()
  in
  loop (Queue.length ct.workq);
  if Queue.is_empty ct.workq
  then (
    let h = Table.create (Table.length tbl) in
    Table.iter (fun k v -> Table.add h k (concrete v)) tbl;
    Ok h)
  else Error (tbl, ct)
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
    = [ 0, Fix (Types.Fn { _from = Fix Types.Bool; _to = Fix Types.Bool })
      ; 1, Fix Types.Bool
      ]
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
  | Ok t ->
    List.of_seq (Table.to_seq t) = [ 0, Fix Types.Bool; 1, Fix Types.Bool ]
;;
