open Syntax

let _or =
  let lhs, rhs = Bind 0, Bind 1 in
  let ssa_0 = If { _if = lhs; _then = rhs; _else = lhs } in
  let ssa_1 = Fn { _from = 1; _to = ssa_0 } in
  Fn { _from = 0; _to = ssa_1 }
;;
