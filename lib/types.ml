type t =
  | Bool
  | Fn of
      { _from : t
      ; _to : t
      }
