(** Represents actions to be taken in the game *)

type direction = 
  | Up
  | Down
  | Left
  | Right

type t = 
  | Move of direction
  | Break
  | Rest

(** [parse c] is the action to be taken when the character [c] is 
    pressed. *)
val parse : char -> t