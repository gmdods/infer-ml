type v = int

type t =
  | Bind of v
  | Let of
      { _let : v
      ; _be : t
      ; _in : t
      }
  | Fn of
      { _from : v
      ; _to : t
      }
  | Call of
      { _fn : t
      ; _with : t
      }
  | If of
      { _if : t
      ; _then : t
      ; _else : t
      }
  | Bool of bool

