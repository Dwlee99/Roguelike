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

(**[move_player t (x, y)] moves the player to [(x, y)].
   Requires: [(x, y)] is empty. *)
let move_player t (x, y) =
  set_tile t t.player_position Empty; set_tile t (x, y) Player;
  t.player_position <- (x, y); t

let update t = function
  (* TODO: Check boundaries. Make sure spot to move to isn't occupied. Attack if
     enemy present. *)
  | Up -> move_player t ((fun (x, y) -> (x, y + 1)) t.player_position)
  | Down -> move_player t ((fun (x, y) -> (x, y - 1)) t.player_position)
  | Left -> move_player t ((fun (x, y) -> (x - 1, y )) t.player_position)
  | Right -> move_player t ((fun (x, y) -> (x + 1, y)) t.player_position)
  | Rest -> t

let init_game width height =
  (** Make the game state random. Put walls in. Maybe randomize player spawn.  
  *)
  let game = Array.make_matrix width height Empty in
  game.(width / 2).(height / 2) <- Player; 
  {
    board = game;
    player_position = (width / 2, height / 2)
  }

let tile_board t = t.board