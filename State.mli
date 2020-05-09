open Board
(** Represents the state of the game. *)

(** Called when the player dies. *)
exception PlayerDeath

(** Type representing the player. *)
type player 

(** Type representing the floor, which holds important stats. *)
type floor 

(** Represents the current player's level. *)
type player_level = int

(** Represents the coordinate system for the board. *)
type coordinate = (int * int)

(** The current data in the game. *)
type t

(** [get_stats player] is a record holding the stats of [player] *)
val get_stats : t -> Messages.player_stats

(** [tile_board t] is an array representing the board on which the game takes
    place. If the returned array is board, then board.(x).(y) will give the tile
    at coordinate (x, y). *)
val tile_board : t -> Board.t

(** [get_player_pos t] returns the coordinates of the player*)
val get_player_pos : t -> int*int

(** [get_board_size t] is the size of the board*)
val get_board_size : t -> int*int

(** [get_msgs t] is the list of strings to be displayed in the messages
    panel in the UI*)
val get_msgs : t -> Messages.msgs

(** [write_msgs t msgs] is the new state produced after the messages [msgs] has 
    been appended to [t.messages]. *)
val write_msgs : t -> string list -> t

(** [write_help t] is the new state after the help instructions have been 
    written to the message board. *)
val write_help : t -> t

(** [write_player_help t] is the new state after the player help instructions
    have been written to the message board. *)
val write_player_help: t -> t

(** [write_fighting_help t] is the new state after the fighting help
    instructions have been written to the message board. *)
val write_fighting_help: t -> t

(** [write_inventory t] is the new state after the inventory has been 
    displayed to the message board. *)
val write_inventory : t -> t

(** [do_display t d (x, y)] displays information about the world 
    based on [d] and offset by [(x, y)] coordinates. *)
val do_display : t -> Action.displayer -> Ascii_panel.t -> (int * int) -> unit

(** [display_update t display] is the updated state [t] after the display 
    [display] has been displayed. *)
val display_update: t -> Action.displayer -> t

(** [turn t action] is the new state of the world after a full turn has been 
    executed on which the player did the action [action]. *)
val do_turn : t -> Action.modifier -> t

(** [init_level width height] is a randomized world with [width] and 
    [height]. *)
val init_level : unit -> t

(** [next_level t] generates a new board that is bigger and contains slightly
    stronger monsters and better weapons/armor*)
val next_level : t -> t
