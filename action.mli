(** Represents actions to be taken in the game *)

(** Represents a direction to do an action in relative to a player. *)
type direction = 
  | Up
  | Down
  | Left
  | Right

(** Represents an action to be taken. *)
type t = 
  | Modify of modifier
  | Display of displayer
  | Quit
  | None

(** Represents actions that modify the state of the game. *)
and modifier = 
  | Move of direction
  | Break
  | Rest
  | Melee_Attack of direction
  | Ranged_Attack of direction

(** Represents actions that merely display information about the game. *)
and displayer = 
  | Help
  | PlayerHelp
  | FightingHelp
  | Inv
  | Melee
  | Ranged

(** [parse c] is the action to be taken when the character [c] is 
    pressed. *)
val parse : char -> t

(** [parse_two c1 c2] is the action to be taken when the character [c1] is
    pressed followed by [c2]. *)
val parse_two : char -> char -> t