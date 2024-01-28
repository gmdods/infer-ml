type 'a t =
  | Bool
  | Fn of
      { _from : 'a
      ; _to : 'a
      }
