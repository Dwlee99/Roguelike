open Action
open Random

type breakable = bool

type tile = 
  | Player
  | Wall of breakable
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
  then t.board.(x).(y) else Wall false

(**[move_player t (x, y)] moves the player to [(x, y)].
   Requires: [(x, y)] is empty. *)
let move_player t (x, y) =
  set_tile t t.player.position Empty; set_tile t (x, y) Player;
  t.player.position <- (x, y)

(** These functions have self-documenting names. *)
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
    for row = -1 to 1 do
      let (x, y) = t.player.position in
      let changed_pos = (x, y + row) in
      if get_tile t changed_pos = Wall true then set_tile t changed_pos Empty
    done;
    for col = -1 to 1 do
      let (x, y) = t.player.position in
      let changed_pos = (x + col, y) in
      if get_tile t changed_pos = Wall true then set_tile t changed_pos Empty
    done;
    t
  | Rest -> t

(** [add_outer_walls board] is a board identical to [board] except with
    each of the tiles that form the outer loop of the board being a wall. *)
let add_outer_walls board = 
  let width = Array.length board in
  let height = Array.length board.(0) in
  for x = 0 to width - 1 do
    board.(x).(0) <- Wall false;
    board.(x).(height - 1) <- Wall false;
  done;
  for y = 1 to height - 2 do
    board.(0).(y) <- Wall false;
    board.(width - 1).(y) <- Wall false;
  done;
  board

(** [empty_board width height] is an empty board with dimensions 
    [width] and [height]. *)
let empty_board width height =
  Array.make_matrix width height Empty

(** [randomize_tiles board] returns a board with the same with and height as 
    [board] but with each tile having an equal chance of being [Empty] or 
    [Wall].

    NOTE: This function was inspired by the roguelike board generating algorithm
    described on a blog post in Trystan's Blog 
    (http://trystans.blogspot.com/2011/08/roguelike-tutorial-03-scrolling-through.html). *)
let randomize_tiles board = 
  Random.self_init ();
  let width = Array.length board in
  let height = Array.length board.(0) in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      board.(x).(y) <- if Random.bool () = true then Empty else Wall true;
    done;
  done;
  board

(** [smooth board] is the board [board] after undergoing one iteration of a 
    wall-smoothing algorithm. 

    NOTE: This function was inspired by the roguelike board generating algorithm
    described on a blog post in Trystan's Blog 
    (http://trystans.blogspot.com/2011/08/roguelike-tutorial-03-scrolling-through.html). *)
let smooth board = 
  let width = Array.length board in
  let height = Array.length board.(0) in
  let new_board = empty_board width height in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      let empties = ref 0 in
      let walls = ref 0 in
      for ox = -1 to 1 do
        for oy = -1 to 1 do
          if x + ox < 0 || x + ox >= width || y + oy < 0 
             || y + oy >= height
          then ()
          else if (board.(x + ox).(y + oy) = Empty)
          then empties := 1 + !empties
          else walls := 1 + !walls
        done
      done;
      new_board.(x).(y) <- if empties >= walls then Empty else Wall true;
    done
  done;
  new_board


(** [gen_board width height] is a randomly generated board with dimensions 
    [width] and [height]. *)
let gen_board width height =
  let empty_board = empty_board width height in
  let random_board = ref (randomize_tiles empty_board) in
  for i = 0 to 5 do
    random_board := smooth !random_board; 
  done;
  let board_with_walls = add_outer_walls !random_board in
  board_with_walls


let init_game width height =
  (** Make the game state random. Put walls in. Maybe randomize player spawn.  
  *)
  let board = gen_board width height in
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