open Action
open Random
open Inventory

exception PlayerDeath

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
  attack: int;
  defense: int;
  inventory : Inventory.t;
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
  monsters : Monster.monster list;
  floor : floor;
}



let get_stats t : Messages.player_stats = {
  level = t.player.level;
  exp = t.player.exp;
  max_exp = t.player.max_exp;
  health = t.player.health;
  max_health = t.player.max_health;
  energy = t.player.energy;
  max_energy = t.player.max_energy;
  turns_played = t.player.turns_played;
  floor = t.floor.floor_num
}

let types_of_monsters = 
  Array.of_list [
    Board.Swordsman; 
    Board.Ranger
  ]

let get_player_pos t = t.player.position

let get_board_size t = (t.floor.board_width, t.floor.board_height)

let tile_board t = Array.map Array.copy t.board

let get_msgs t = t.messages

let write_msgs t msgs = 
  { t with messages = Messages.write_msgs msgs t.messages; }

let write_help t = {t with messages = Messages.write_help t.messages}

let get_printable_inv (i:Inventory.t) : Messages.inventory = 
  {
    melee = Inventory.get_melee_name i;
    ranged = get_ranged_name i;
    head = get_head_name i;
    torso = get_torso_name i;
    legs = get_legs_name i;
    feet = get_feet_name i;
    items = get_items_names i;
    max_items = get_max_items i;
  }

let write_inventory t = {
  t with 
  messages = (Messages.write_inventory 
                (get_printable_inv t.player.inventory) t.messages)}

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

let take_damage t damage m_name =
  {t with 
   player = {t.player with 
             health = t.player.health - damage
            };
   messages = if damage = 0 then t.messages else
       Messages.write_msg 
         (m_name ^ " did " ^ (string_of_int damage) ^ " to you!") 
         t.messages
  }

(** These functions have self-documenting names. *)
let up_one (x, y) = (x, y + 1)

let down_one (x, y) = (x, y - 1)

let right_one (x, y) = (x + 1, y)

let left_one (x, y) = (x - 1, y)


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
let rec add_monsters (monsters : Monster.monster list) (state : t) =
  match monsters with 
  | [] -> state
  | h::t -> add_monsters t 
              (let monster_board = place_entity (Monster (Monster.get_type h)) 
                   state.board in 
               let board = fst monster_board in 
               let monster_loc = snd monster_board in 
               let monster = {h with position = monster_loc} in 
               let new_state = 
                 {state with board = board; 
                             monsters = monster::(state.monsters)} in 
               new_state)

let random_element arr =
  Array.get arr (Random.int (Array.length arr))

(** [create_monsters num strength] is a list of [num] monsters with level 
    strength parameter [strength]. *)
let rec create_monsters num strength =
  if num < 0 then failwith "Cannot have a negative number of monsters!"
  else match num with 
    | 0 -> []
    | k -> begin
        let monster_type = random_element types_of_monsters in
        match monster_type with
        | Board.Swordsman ->
          let monster = Swordsman.Swordsman.create_monster strength in 
          monster :: (create_monsters (num - 1) strength)
        | Board.Ranger ->
          let monster = Ranger.Ranger.create_monster strength in 
          monster :: (create_monsters (num - 1) strength)
      end

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

let add_stairs t =
  let stairs_board = place_entity Stairs t.board in 
  let new_board = fst stairs_board in 
  {t with board = new_board}

let next_level t =
  let floor = get_floor (t.floor.floor_num + 1) in
  let width = floor.board_width in 
  let height = floor.board_height in 
  let raw_board = Board.gen_board width height in
  let player_and_board = place_entity Player raw_board in 
  let board = fst player_and_board in 
  let player_loc = snd player_and_board in
  let new_state = {
    player = {t.player with position = player_loc};
    board = board;
    messages = [];
    floor = floor;
    monsters = [];
  } in
  let state_with_monsters = add_monsters 
      (create_monsters floor.num_monsters floor.monster_strength) new_state in 
  let state_with_stairs = add_stairs state_with_monsters in
  state_with_stairs

let init_level =
  let floor = get_floor 1 in
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
        attack = 3;
        defense = 3;
        inventory = Inventory.init_inv
      };
      monsters = [];
      floor = floor;
    } in 
  let state_with_monsters = add_monsters 
      (create_monsters floor.num_monsters floor.monster_strength) init_state in 
  let state_with_stairs = add_stairs state_with_monsters in
  state_with_stairs

(* [move t] is the state of the game after the movement of a player in a 
   given direction. 
   It prevents movement if the player lacks the energy to do so.*)
let move dir t = 
  if t.player.energy < move_cost 
  then write_msgs t ["You do not have enough energy to move. Try resting."]
  else (
    let new_energy = t.player.energy - move_cost in
    let new_pos = match dir with
      | Up -> up_one t.player.position
      | Down -> down_one t.player.position
      | Left -> left_one t.player.position
      | Right -> right_one t.player.position
    in 
    let attempt_tile = Board.get_tile t.board new_pos in
    match attempt_tile with
    | Empty -> move_player t new_pos |> inc_turns |> set_energy new_energy
    | Stairs -> next_level t
    | _ -> t
  )

(* [break t] returns the state of the game after the player breaks the four
   blocks around them *)
let break t = 
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

(*[rest t] is the state of the game after the player rests 1 turn *)
let rest t = 
  let new_energy = min (t.player.energy + rest_gain) t.player.max_energy in
  inc_turns t |> set_energy new_energy

let damage_monster t (x, y) damage = 
  {t with monsters = List.map (
       fun (m : Monster.monster) -> if m.position = (x, y) then 
           {m with health = m.health - damage} else m) t.monsters}

let get_attack_spots weapon dir =
  match Weapon.get_type weapon with
  | Short_Sword -> Short_sword.Short_Sword.attack weapon dir
  | Short_Bow -> Short_bow.Short_Bow.attack weapon dir
  | Battleaxe -> Battleaxe.Battleaxe.attack weapon dir

let attack_weapon t weapon dir =
  let (pX, pY) = t.player.position in
  let attack_spots = get_attack_spots weapon dir in List.fold_left 
    (fun t (relX, relY, damage) -> 
       damage_monster t (pX + relX, pY + relY) damage) t attack_spots


let attack_melee t dir = 
  match Inventory.get_melee_weapon t.player.inventory with
  | Some weapon -> attack_weapon t weapon dir
  | None -> t

let attack_ranged t dir = 
  match Inventory.get_ranged_weapon t.player.inventory with
  | Some weapon -> attack_weapon t weapon dir
  | None -> t

(** [do_player_turn t action] is the state of the board after a player's turn
    has been executed on which the player did the action [action]. *)
let do_player_turn t action =
  (* Attack if enemy present. *)
  match action with
  | Move direction -> move direction t
  | Break -> break t
  | Help -> write_help t;
  | Rest -> rest t;
  | Inv -> write_inventory t
  | Display_Melee -> failwith "Unimplemented."
  | Display_Ranged -> failwith "Unimplemented."
  | Melee_Attack dir -> attack_melee t dir
  | Ranged_Attack dir -> attack_ranged t dir
  | None -> failwith "Unimplemented."


(** [move_monster c_pos (x,y) m_type m t] is the state after monster m moves
    from c_pos to (x,y) *)
let move_monster c_pos (x, y) m_type (m : Monster.monster) t =
  if Board.get_tile t.board (x, y) = Empty then (
    Board.set_tile t.board c_pos Empty; 
    Board.set_tile t.board (x, y) (Monster m_type);
    m)
  else
    {m with position = c_pos}

(** [kill_monster m t rest_of_monsters] is the state of the game after 
    monster [m] is killed*)
let kill_monster (m:Monster.monster) t rest_of_monsters = 
  t.board.(fst m.position).(snd m.position) <- Empty;
  {t with 
   monsters = rest_of_monsters;
   messages = Messages.write_msg (m.name ^ "has died. Woohoo.") t.messages;
  }

(** [do_monster_turn t] is the state after all the monsters take their turn *)
let do_monster_turn t =
  let rec turns t (monsters : Monster.monster list) acc =
    match monsters with
    | h::tail -> begin
        if h.health <= 0 then turns (kill_monster h t tail) tail acc
        else 
          (match Monster.get_type h with
           | Board.Swordsman ->
             let (new_m, damage) = 
               Swordsman.Swordsman.do_turn h t.board t.player.position in
             specific_turn h tail new_m damage t Board.Swordsman acc
           | Board.Ranger ->
             let (new_m, damage) = 
               Ranger.Ranger.do_turn h t.board t.player.position in
             specific_turn h tail new_m damage t Board.Ranger acc )
      end
    | [] -> (t, acc)
  and specific_turn m tail new_m damage t m_type acc =
    let updated_t = take_damage t damage m.name in
    let final_monster = 
      move_monster m.position new_m.position m_type new_m updated_t 
    in
    turns updated_t tail (final_monster :: acc)
  in
  let (new_t, new_monsters) = turns t t.monsters [] in
  {new_t with monsters = new_monsters}

let do_turn t action = 
  let new_state = (do_player_turn t action) |> do_monster_turn in 
  if new_state.player.health <= 0 then raise PlayerDeath else new_state