(** Represents actions to be taken in the game *)

type t = 
  | Left
  | Right
  | Up
  | Down
  | Rest

(** [parse c] is the action to be taken when the character [c] is 
    pressed. *)
val parse : char -> t