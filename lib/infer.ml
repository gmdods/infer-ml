open Syntax
module Table = Hashtbl.Make (Int)

type index = int

type cell =
  | Ctor of cell Types.t
  | Ref of index

type c =
  | Eq of
      { lhs : cell
      ; rhs : cell
      }

type t =
  { nvar : int ref
  ; workq : c Queue.t
  }

let create () =
  let workq = Queue.create () in
  { nvar = ref 0; workq }
;;

let cell { nvar; _ } =
  let index = !nvar in
  incr nvar;
  Ref index
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

let _true =
  let ssa_0 = Fn { _from = 1; _to = Bind 1 } in
  let ssa_1 = Call { _fn = Bind 0; _with = Bool true } in
  Let { _let = 0; _be = ssa_0; _in = ssa_1 }
;;

let _or =
  let lhs, rhs = Bind 0, Bind 1 in
  let ssa_0 = If { _if = lhs; _then = rhs; _else = lhs } in
  let ssa_1 = Fn { _from = 1; _to = ssa_0 } in
  Fn { _from = 0; _to = ssa_1 }
;;
