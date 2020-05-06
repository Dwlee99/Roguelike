open Action
open Random
open Messages
open Board

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

let move_cost = 1

let rest_gain = 1

let break_cost = 10

let help_strings = 
  ["INSTRUCTIONS/CONTROLS:"; 
   "Press i,j,k,l to move up, left, down, right"; 
   "Press b to break the 4 walls near you"; 
   "Press [spacebar] to rest"; 
   "Press h to see instructions again."]

type t = {
  board: Board.t;
  messages: string list;
  player: player;
}

let get_stats player = {
  level = player.level;
  exp = player.exp;
  max_exp = player.max_exp;
  health = player.health;
  max_health = player.max_health;
  energy = player.energy;
  max_energy = player.max_energy;
  turns_played = player.turns_played
}

let tile_board t = Array.map Array.copy t.board

let get_player t = t.player

let get_msgs t = t.messages

let write_msg t msg = {
  board = t.board;
  messages = Messages.write_msg msg (get_msgs t);
  player = t.player;
}

let write_msgs t msgs = 
  let rec helper t = function
    | [] -> t;
    | h :: tail -> helper (write_msg t h) tail
  in helper t msgs


(**[move_player t (x, y)] moves the player to [(x, y)].
   Requires: [(x, y)] is empty. *)
let move_player t (x, y) =
  Board.set_tile t.board t.player.position Empty; 
  Board.set_tile t.board (x, y) Player;
  {t with player = {t.player with position = (x, y)}}

let inc_turns t =
  {t with player = {t.player with turns_played = t.player.turns_played + 1}}

let set_energy e t = 
  {t with player = {t.player with energy = e}}

(** These functions have self-documenting names. *)
let up_one (x, y) = (x, y + 1)

let down_one (x, y) = (x, y - 1)

let right_one (x, y) = (x + 1, y)

let left_one (x, y) = (x - 1, y)

let update t action =
  (* Attack if enemy present. *)
  match action with
  | Move direction -> 
    if t.player.energy < move_cost 
    then write_msg t "You do not have enough energy to move. Try resting."
    else (
      let new_energy = t.player.energy - move_cost in
      let new_pos = match direction with
        | Up -> up_one t.player.position
        | Down -> down_one t.player.position
        | Left -> left_one t.player.position
        | Right -> right_one t.player.position
      in 
      if Board.get_tile t.board new_pos = Empty then 
        move_player t new_pos |> inc_turns |> set_energy new_energy
      else t 
    )
  | Break ->
    if t.player.energy < break_cost 
    then 
      write_msg t "You do not have enough energy to break walls. Try resting."
    else (
      let new_energy = t.player.energy - break_cost in
      for row = -1 to 1 do
        let (x, y) = t.player.position in
        let changed_pos = (x, y + row) in
        if Board.get_tile t.board changed_pos = Wall true 
        then Board.set_tile t.board changed_pos Empty
      done;
      for col = -1 to 1 do
        let (x, y) = t.player.position in
        let changed_pos = (x + col, y) in
        if Board.get_tile t.board changed_pos = Wall true 
        then Board.set_tile t.board changed_pos Empty
      done;
      inc_turns t |> set_energy new_energy
    )
  | Help -> write_msgs t (help_strings);
  | Rest -> 
    let new_energy = min (t.player.energy + rest_gain) t.player.max_energy in
    inc_turns t |> set_energy new_energy


(** [player_location board] is a location that is surrounded by a layer of empty 
    tiles, which thus would be suitable for the player to spawn on. *)
let rec player_location board =
  Random.self_init ();
  let width = Array.length board in
  let height = Array.length board.(0) in
  let x = 1 + (Random.int (width - 2)) in 
  let y = 1 + (Random.int (height - 2)) in 
  let suitable = ref true in 
  for ox = -1 to 1 do
    for oy = -1 to 1 do
      suitable := (board.(x + ox).(y + oy) = Empty) && !suitable
    done
  done;
  if !suitable then (x,y) else player_location board

(** [place_player board] is the board [board] with a player added in a location 
    suitable location (one in which the player is not surrounded by walls). *)
let place_player board =
  let position = player_location board in 
  let x = fst position in 
  let y  = snd position in
  board.(x).(y) <- Player;
  (board, (x,y))

let init_game width height =
  let raw_board = Board.gen_board width height in
  let player_and_board = place_player raw_board in 
  let board = fst player_and_board in 
  let player_loc = snd player_and_board in
  {
    board = board;
    messages = [];
    player = {
      position = player_loc;
      level = 1;
      exp = 0;
      max_exp = 10;
      health = 10;
      max_health = 10;
      energy = 100;
      max_energy = 100;
      turns_played = 0;
    }
  }