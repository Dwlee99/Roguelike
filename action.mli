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
  | Help
  | Inv
  | Display_Melee
  | Display_Ranged
  | Melee_Attack of direction
  | Ranged_Attack of direction
  | None

(** [parse c] is the action to be taken when the character [c] is 
    pressed. *)
val parse : char -> t

val parse_two : char -> char -> t