type v = int

type 'a t =
  | Bind of 'a
  | Let of
      { _let : 'a
      ; _be : 'a t
      ; _in : 'a t
      }
  | Fn of
      { _from : 'a
      ; _to : 'a t
      }
  | Call of
      { _fn : 'a t
      ; _with : 'a t
      }
  | If of
      { _if : 'a t
      ; _then : 'a t
      ; _else : 'a t
      }
  | Bool of bool

module Types = struct
  type 'a t =
    | Void of 'a
    | Bool
    | Fn of
        { _from : 'a t
        ; _to : 'a t
        }
end
