open Action

type tile = 
  | Player
  | Wall
  | Empty

type t = {
  mutable board: tile array array;
  mutable player_position: (int * int);
}

(** Sets the tile at [(x, y)] to [tile]. *)
let set_tile t (x, y) tile =
  t.board.(x).(y) <- tile

(** Gets the contents of the tile at [(x, y)].
    If [(x, y)] is out of bounds, the result is [Wall]. *)
let get_tile t (x, y) = 
  let width = Array.length t.board in
  let height = Array.length t.board.(0) in 
  if x >= 0 && x < width && y >= 0 && y < height 
  then t.board.(x).(y) else Wall

(**[move_player t (x, y)] moves the player to [(x, y)].
   Requires: [(x, y)] is empty. *)
let move_player t (x, y) =
  set_tile t t.player_position Empty; set_tile t (x, y) Player;
  t.player_position <- (x, y)

let update t action =
  (* Attack if enemy present. *)
  let new_pos = match action with
    | Up -> ((fun (x, y) -> (x, y + 1)) t.player_position)
    | Down -> ((fun (x, y) -> (x, y - 1)) t.player_position)
    | Left -> ((fun (x, y) -> (x - 1, y )) t.player_position)
    | Right -> ((fun (x, y) -> (x + 1, y)) t.player_position)
    | Rest -> t.player_position
  in if get_tile t new_pos = Empty then (move_player t new_pos; t) else
    t

let init_game width height =
  (** Make the game state random. Put walls in. Maybe randomize player spawn.  
  *)
  let board = Array.make_matrix width height Empty in
  board.(width / 2).(height / 2) <- Player; 
  {
    board = board;
    player_position = (width / 2, height / 2)
  }

let tile_board t = t.board