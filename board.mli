(** Represents the game board. *)

(** Whether a wall is breakable. *)
type breakable = bool

type monster_type = 
  | Swordsman

(** The things that can occupy coordinates on the board. *)
type tile = 
  | Player
  | Wall of breakable
  | Empty
  | Monster of monster_type

(** The game board. *)
type t = tile array array

(** Sets the tile at [(x, y)] to [tile]. *)
val set_tile : t -> int * int -> tile -> unit

(** Gets the contents of the tile at [(x, y)].
    If [(x, y)] is out of bounds, the result is [Wall]. *)
val get_tile : t -> int * int -> tile

val in_bound : t -> (int * int) -> bool

(** [tile_board t] is an array representing the board on which the game takes
    place. If the returned array is board, then board.(x).(y) will give the tile
    at coordinate (x, y). *)
val tile_board : t -> tile array array

(** [gen_board width height] is a randomly generated board with dimensions 
    [width] and [height]. *)
val gen_board : int -> int -> t

(** [path_to_player board c_pos t_pos] is the shortest path to n_pos from m_pos.
    If there is no path, it returns [None]. *)
val path_to : t -> (int * int) -> (int * int) -> (int * int) list option