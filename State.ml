open Action
open Random

(** The energy cost of moving one tile. *)
let move_cost = 1

(** The energy gained by resting for a turn. *)
let rest_gain = 1

(** The energy cost of executing a break action. *)
let break_cost = 10

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

type floor = {
  floor_num : int;
  board_width : int;
  board_height : int;
  monster_strength: int;
  num_monsters: int;
}

type t = {
  board : Board.t;
  messages : Messages.msgs;
  player : player;
  monsters : monster list;
  floor : floor;
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

let get_player_pos t = t.player.position

let get_board_size t = (t.floor.board_width, t.floor.board_height)

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
      write_msgs t 
        ["You do not have enough energy to break walls. Try resting."]
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


(** [spawn_location board] is a location that is surrounded by a layer of 
    empty tiles, which thus would be suitable for the player or monster to 
    spawn on. *)
let rec spawn_location board =
  Random.self_init ();
  let width = Array.length board in
  let height = Array.length board.(0) in
  let x = 1 + (Random.int (width - 2)) in 
  let y = 1 + (Random.int (height - 2)) in 
  let suitable = ref true in 
  for ox = -1 to 1 do
    for oy = -1 to 1 do
      suitable := (board.(x + ox).(y + oy) = Board.Empty) && !suitable
    done
  done;
  if !suitable then (x,y) else spawn_location board

(** [place_player board] is the board [board] with a player added in a location 
    suitable location (one in which the player is not surrounded by walls). *)
let place_entity tile_type board =
  let position = spawn_location board in 
  let x = fst position in 
  let y  = snd position in
  board.(x).(y) <- tile_type;
  (board, (x,y))

(** [add_monsters monsters t] is the state [t] updated with the monsters 
    [monsters] added to the game. *)
let rec add_monsters (monsters : monster list) (state : t) =
  match monsters with 
  | [] -> state
  | h::t -> add_monsters t 
              (let monster_board = place_entity Monster state.board in 
               let board = fst monster_board in 
               let monster_loc = snd monster_board in 
               let monster = {h with position = monster_loc} in 
               let new_state = 
                 {state with board = board; 
                             monsters = monster::(state.monsters)} in 
               new_state)

(** [create_monsters num strength] is a list of [num] monsters with level 
    strength parameter [strength]. *)
let rec create_monsters num strength =
  if num < 0 then failwith "Cannot have a negative number of monsters!"
  else match num with 
    | 0 -> []
    | k -> 
      let monster = {
        name = Name.random_name ();
        position = (0, 0);
        health = 10 * strength;
        max_health = 10 * strength;
        damage = 2 * strength;
      } in 
      monster :: (create_monsters (num - 1) strength)

(** [get_floor floor_num] is the floor corresponding to the floor number
    [floor_num]. *)
let get_floor floor_num = 
  let board_width = 80 + floor_num * 5 in 
  let board_height = 36 + floor_num * 2 in
  let monster_strength = 10 + floor_num in 
  let num_monsters = 10 + 2 * floor_num in
  {
    floor_num = floor_num;
    board_width = board_width;
    board_height = board_height;
    monster_strength = monster_strength;
    num_monsters = num_monsters;
  }

let init_game floor_num =
  let floor = get_floor 5 in
  let width = floor.board_width in 
  let height = floor.board_height in 
  let raw_board = Board.gen_board width height in
  let player_and_board = place_entity Player raw_board in 
  let board = fst player_and_board in 
  let player_loc = snd player_and_board in
  let init_state =
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
        energy = 10000;
        max_energy = 10000;
        turns_played = 0;
      };
      monsters = [];
      floor = floor;
    } in 
  let state_with_monsters = add_monsters 
      (create_monsters floor.num_monsters floor.monster_strength) init_state in 
  state_with_monsters