open Action

type tile = 
  | Player
  | Wall
  | Empty

type player = {
  mutable position : (int*int);
  mutable level : int;
  mutable exp : int;
  mutable max_exp : int;
  mutable health : int;
  mutable max_health : int;
  mutable energy : int;
  mutable max_energy : int;
}

type t = {
  board: tile array array;
  player: player;
}

(** Sets the tile at [(x, y)] to [tile]. *)
let set_tile t (x, y) tile =
  let width = Array.length t.board in
  let height = Array.length t.board.(0) in 
  if x >= 0 && x < width && y >= 0 && y < height 
  then t.board.(x).(y) <- tile

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
  set_tile t t.player.position Empty; set_tile t (x, y) Player;
  t.player.position <- (x, y)

let up_one (x, y) = (x, y + 1)

let down_one (x, y) = (x, y - 1)

let right_one (x, y) = (x + 1, y)

let left_one (x, y) = (x - 1, y)


let update t action =
  (* Attack if enemy present. *)
  match action with
  | Move direction -> 
    let new_pos = match direction with
      | Up -> up_one t.player.position
      | Down -> down_one t.player.position
      | Left -> left_one t.player.position
      | Right -> right_one t.player.position
    in if get_tile t new_pos = Empty then (move_player t new_pos; t) else
      t
  | Break ->
    for col = -1 to 1 do
      for row = -1 to 1 do
        let (x, y) = t.player.position in
        let changed_pos = (x + col, y + row) in
        if get_tile t changed_pos = Wall then set_tile t changed_pos Empty
      done
    done;
    t
  | Rest -> t






(** [add_outer_walls board] is a board identical to [board] except with
    each of the tiles that form the outer loop of the board being a wall. *)
let add_outer_walls board = 
  let width = Array.length board in
  let height = Array.length board.(0) in
  for x = 0 to width - 1 do
    board.(x).(0) <- Wall;
    board.(x).(height - 1) <- Wall;
  done;
  for y = 1 to height - 2 do
    board.(0).(y) <- Wall;
    board.(width - 1).(y) <- Wall;
  done;
  board


let init_game width height =
  (** Make the game state random. Put walls in. Maybe randomize player spawn.  
  *)
  let board = Array.make_matrix width height Empty in
  let board = add_outer_walls board in
  board.(width / 2).(height / 2) <- Player; 
  {
    board = board;
    player = {
      position = (width / 2, height / 2);
      level = 1;
      exp = 0;
      max_exp = 10;
      health = 10;
      max_health = 10;
      energy = 100;
      max_energy = 100;
    }
  }

let tile_board t = t.board