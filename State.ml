open Action
open Random
open Board

(** The energy cost of moving one tile. *)
let move_cost = 1

(** The energy gained by resting for a turn. *)
let rest_gain = 1

(** The energy cost of executing a break action. *)
let break_cost = 10

type floor = int

type player_level = int

type coordinate = (int * int)

type player = {
  position : coordinate;
  level : player_level;
  exp : int;
  max_exp : int;
  health : int;
  max_health : int;
  energy : int;
  max_energy : int;
  turns_played : int;
}

type monster = {
  name : string;
  position : coordinate;
  health : int;
  max_health : int;
  damage : int;
}

type t = {
  floor : floor;
  board : Board.t;
  messages : Messages.msgs;
  player : player;
  monsters : monster list;
}



let get_stats player : Messages.player_stats = {
  level = player.level;
  exp = player.exp;
  max_exp = player.max_exp;
  health = player.health;
  max_health = player.max_health;
  energy = player.energy;
  max_energy = player.max_energy;
  turns_played = player.turns_played
}

let get_player t = t.player

let tile_board t = Array.map Array.copy t.board

let get_msgs t = t.messages

let write_msgs t msgs = 
  { t with messages = Messages.write_msgs msgs t.messages; }

let write_help t = {t with messages = Messages.write_help t.messages}

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

(** [do_player_turn t action] is the state of the board after a player's turn
    has been executed on which the player did the action [action]. *)
let do_player_turn t action =
  (* Attack if enemy present. *)
  match action with
  | Move direction -> 
    if t.player.energy < move_cost 
    then write_msgs t ["You do not have enough energy to move. Try resting."]
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
      write_msgs t ["You do not have enough energy to break walls. Try resting."]
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
  | Help -> write_help t;
  | Rest -> 
    let new_energy = min (t.player.energy + rest_gain) t.player.max_energy in
    inc_turns t |> set_energy new_energy



let do_turn t action = 
  let player_turn = do_player_turn t action in 
  player_turn


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
    floor = 1;
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
    };
    monsters = [];
  }