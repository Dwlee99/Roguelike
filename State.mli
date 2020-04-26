(** Represents the state of the game. *)

type breakable = bool

(** The things that can occupy coordinates on the board. *)
type tile = 
  | Player
  | Wall of breakable
  | Empty

type player = {
  position : (int*int);
  level : int;
  exp : int;
  max_exp : int;
  health : int;
  max_health : int;
  energy : int;
  max_energy : int;
  turns_played : int;
}

(** The current data in the game. *)
type t

(** [get_stats player] is a record holding the stats of [player] *)
val get_stats : player -> Messages.player_stats

(** [update t action] is the new state of the world following [action] being
    performed. *)
val update : t -> Action.t -> t

(** [init_game width height] is a randomized world with [width] and [height]. *)
val init_game : int -> int -> t

(** [tile_board t] is an array representing the board on which the game takes
    place. If the returned array is board, then board.(x).(y) will give the tile
    at coordinate (x, y). *)
val tile_board : t -> tile array array

(** [get_player t] is the record representing data of the player in the
    state t*)
val get_player : t -> player

(** [get_msgs t] is the list of strings to be displayed in the messages
    panel in the UI*)
val get_msgs : t -> string list

(** [write_msg t msg] is the new state produced after the message [msg] has been 
    added to [t]. *)
val write_msg : t -> string -> t