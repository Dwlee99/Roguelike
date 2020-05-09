(** Represents actions to be taken in the game *)

type direction = 
  | Up
  | Down
  | Left
  | Right

type t = 
  | Modify of modifier
  | Display of displayer
  | Quit
  | None
and modifier = 
  | Move of direction
  | Break
  | Rest
  | Melee_Attack of direction
  | Ranged_Attack of direction
and
  displayer = 
  | Help
  | Inv
  | Melee
  | Ranged

(** [parse c] is the action to be taken when the character [c] is 
    pressed. *)
val parse : char -> t

val parse_two : char -> char -> t