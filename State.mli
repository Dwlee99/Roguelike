(** Represents the state of the game. *)

(** The things that can occupy coordinates on the board. *)
type tile = 
  | Player
  | Wall
  | Empty

(** The current data in the game. *)
type t


(** [update t action] is the new state of the world following [action] being
    performed. *)
val update : t -> Action.t -> t

(** [init_game width height] is a randomized world with [width] and [height]. *)
val init_game : int -> int -> t

(** [tile_board t] is an array representing the board on which the game takes
    place. If the returned array is board, then board.(x).(y) will give the tile
    at coordinate (x, y). *)
val tile_board : t -> tile array array