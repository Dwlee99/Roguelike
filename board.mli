(** Represents the game board. *)

(** Whether a wall is breakable. *)
type breakable = bool

(** The type of a monster. *)
type monster_type = 
  | Swordsman
  | Ranger

(** The type of a weapon. *)
type weapon_type =
  | ShortSword
  | ShortBow
  | BattleAxe

(** The things that can occupy coordinates on the board. *)
type tile = 
  | Player
  | Wall of breakable
  | Empty
  | Monster of monster_type
  | Stairs
  | Weapon of weapon_type

(** The game board. *)
type t = tile array array

(** [tile_board t] is an array representing the board on which the game takes
    place. If the returned array is board, then board.(x).(y) will give the tile
    at coordinate (x, y). *)
val tile_board : t -> tile array array

(** Sets the tile at [(x, y)] to [tile]. *)
val set_tile : t -> int * int -> tile -> unit

(** Gets the contents of the tile at [(x, y)].
    If [(x, y)] is out of bounds, the result is [Wall]. *)
val get_tile : t -> int * int -> tile

(** [in_bound t (x,y)] is true if the coordinate [(x,y)] is in bounds in
    the board [t], false otherwise. *)
val in_bound : t -> (int * int) -> bool

(** [gen_board width height] is a randomly generated board with dimensions 
    [width] and [height]. *)
val gen_board : int -> int -> t

(** [direction_to board cpos fpos max_dist] is [Some dir] if dir is the 
    direction that one would move to get from [cpos] to [fpos] on the board 
    [board]. Returns [None] if there is no parth within max_dist] moves. *)
val direction_to : t -> (int * int) -> (int * int) -> int -> 
  Action.direction option