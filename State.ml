open Action
open Random
open Inventory
open Math

exception PlayerDeath

let () = Random.self_init ()

(** The energy cost of moving one tile. *)
let move_cost = 1

(** The energy gained by resting for a turn. *)
let rest_gain = 1

(** The energy cost of executing a break action. *)
let break_cost = 10

(** The energy cost of executing an attack action. *)
let attack_cost = 3

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
  weapon_strength: int;
  num_weapons: int;
  num_armor: int;
  armor_strength: int;
}

type t = {
  board : Board.t;
  messages : Messages.msgs;
  player : player;
  monsters : Monster.monster list;
  floor : floor;
  weapons : Weapon.weapon list;
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

(** A list of the types of weapons in the game. *)
let types_of_weapons = 
  Array.of_list [
    Board.ShortSword; 
    Board.ShortBow;
    Board.BattleAxe
  ]

(** A list of the types of monsters in the game. *)
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

let write_player_help t = 
  {t with messages = Messages.write_player_help t.messages}

let write_fighting_help t =
  {t with messages = Messages.write_fighting_help t.messages}

(** [get_printable_inv i] is a printable version of the inventory [i]. *)
let get_printable_inv (i:Inventory.t) : Messages.inventory = 
  {
    melee = Inventory.get_melee_name i;
    ranged = get_ranged_name i;
    armor = get_armor_name i;
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

(** [inc_turns t] is [t] with turns_played increased by 1. *)
let inc_turns t =
  {t with player = {t.player with turns_played = t.player.turns_played + 1}}

(** [set_energy e t] is the state [t] with the player's energy set to [e]. *)
let set_energy e t = 
  {t with player = {t.player with energy = e}}

(** [take_damage t damage m_name] is the state [t] with the player taking 
    [damage] damage from the monster of name [m_name]. *)
let take_damage t damage m_name =
  if damage = 0 then t
  else 
    let p = float_of_int (Inventory.get_armor_protection t.player.inventory) in 
    if (Random.float 1.0) < (p /. (7.0 +. (3.0 *. p))) then write_msgs t 
        ["Your armor has blocked " ^ (string_of_int damage) ^ " damage."]
    else 
      {t with 
       player = {t.player with 
                 health = t.player.health - damage
                };
       messages =
         Messages.write_msg 
           (m_name ^ " did " ^ (string_of_int damage) ^ " to you!") 
           t.messages
      }

(** [exp_to_level_up level] is the amount of experience needed to level up
    for the level [level]. *)
let exp_to_level_up level = 
  if level <= 15 then 2 * level + 7 else
  if level <= 30 then 5 * level - 38 else
    9 * level - 158

(** [max_hp p_level] is the max health of a player at level [p_level]. *)
let max_hp p_level = 2 * (p_level - 1) + 10

(** [max_energy p_level] is the max energy of a player at level [p_level]. *)
let max_energy p_level = 40 * (p_level - 1) + 200

(** [level_up t exp] is t with [t.player] having leveled up and had [t.exp] set
    to [exp].*)
let level_up t exp = 
  let new_l = t.player.level + 1 in
  let max_hp = max_hp new_l in
  let max_energy = max_energy new_l in
  { t with player = 
             {t.player with exp = exp;
                            level = new_l;
                            max_exp = exp_to_level_up new_l;
                            max_health = max_hp;
                            health = max_hp;
                            max_energy = max_energy;
                            energy = max_energy;
             }
  }

(** [add_exp t exp] is the state [t] with [exp] exp added to the player's 
    experience. *)
let rec add_exp t exp =
  let n_exp = t.player.exp + exp in
  let max_exp = t.player.max_exp in
  if n_exp < t.player.max_exp then 
    {t with player = {t.player with exp = n_exp}}
  else let new_t = level_up t (n_exp - max_exp) in add_exp new_t 0    

(** [spawn_location board] is a location that is surrounded by a layer of 
    empty tiles, which thus would be suitable for the player or monster to 
    spawn on. *)
let rec spawn_location board =
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

(** [add_monsters monsters state] is the state [state] updated with the monsters 
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

(** [random_element arr] is a random element of array [arr]. *)
let random_element arr =
  Array.get arr (Random.int (Array.length arr))

(** [create_monsters num strength] is a list of [num] monsters with 
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

(** [add_weapons weapons state] is the state [state] with the weapons 
    [weapons] added to the game. *)
let rec add_weapons weapons state =
  match weapons with 
  | [] -> state
  | h :: t -> 
    add_weapons t
      (let weapon_board = place_entity (Weapon (Weapon.get_type h)) 
           state.board in 
       let board = fst weapon_board in 
       let weapon_loc = snd weapon_board in 
       let weapon = {h with position = Some weapon_loc} in 
       let new_state =
         {state with board = board; weapons = weapon::(state.weapons)} in 
       new_state)

(** [create_weapons num strength] is a list o f[num] weapons with strength 
    parameter [strength]. *)
let rec create_weapons num strength =
  if num < 0 then failwith "Cannot have a negative number of weapons."
  else match num with
    | 0 -> []
    | k ->
      (let weapon_type = random_element types_of_weapons in 
       match weapon_type with
       | Board.ShortSword -> 
         let weapon = Short_sword.Short_Sword.create_weapon strength in 
         weapon :: (create_weapons (num - 1) strength)
       | Board.ShortBow -> 
         let weapon = Short_bow.Short_Bow.create_weapon strength in 
         weapon :: (create_weapons (num - 1) strength)
       | Board.BattleAxe -> 
         let weapon = Battleaxe.Battleaxe.create_weapon strength in 
         weapon :: (create_weapons (num - 1) strength))

(** [add_armor num state] is the state [state] with [num] pieces of armor
    added to the board. *)
let rec add_armor num state =
  if num < 0 then failwith "cannot have a negative number of armor pieces."
  else match num with
    | 0 -> state
    | k -> add_armor (num - 1) 
             (let armor_board = place_entity (Armor (state.floor.floor_num)) 
                  state.board in 
              let board = fst armor_board in 
              let new_state = {state with board = board} in 
              new_state)

(** [get_floor floor_num] is the floor corresponding to the floor number
    [floor_num]. *)
let get_floor floor_num = 
  let board_width = 80 + floor_num * 5 in 
  let board_height = 36 + floor_num * 2 in
  {
    floor_num = floor_num;
    board_width = board_width;
    board_height = board_height;
    monster_strength = floor_num;
    num_monsters = 10 + 2 * floor_num;
    num_weapons = 4 + (floor_num / 2);
    weapon_strength = floor_num;
    num_armor = 2 + (floor_num / 3);
    armor_strength = 1 + floor_num;
  }

(** [add_stairs t] is the state [t] with the stairs added to the board. *)
let add_stairs t =
  let stairs_board = place_entity Stairs t.board in 
  let new_board = fst stairs_board in 
  {t with board = new_board}

(** [next_level t] is the state [t] advanced to the next floor (or level) of
    of the game. *)
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
    weapons = [];
  } in
  let state_with_monsters = add_monsters 
      (create_monsters floor.num_monsters floor.monster_strength) new_state in 
  let state_with_stairs = add_stairs state_with_monsters in
  let state_with_weapons = 
    add_weapons (create_weapons floor.num_weapons 
                   floor.weapon_strength) state_with_stairs in 
  let state_with_armor = add_armor floor.num_armor state_with_weapons in 
  state_with_armor

(** [make_init_state board pLoc floor] is the initial state of the game with 
    the baord being [board], the player being at the location [pLoc], and the 
    floor being [floor]. *)
let make_init_state board pLoc floor = {
  board = board;
  messages = [];
  player = {
    position = pLoc;
    level = 1;
    exp = 0;
    max_exp = exp_to_level_up 1;
    health = 10;
    max_health = 10;
    energy = 200;
    max_energy = 200;
    turns_played = 0;
    attack = 3;
    defense = 3;
    inventory = Inventory.init_inv
  };
  monsters = [];
  floor = floor;
  weapons = [];
}

let init_level () =
  let floor = get_floor 1 in
  let width = floor.board_width in 
  let height = floor.board_height in 
  let raw_board = Board.gen_board width height in
  let player_and_board = place_entity Player raw_board in 
  let board = fst player_and_board in 
  let player_loc = snd player_and_board in
  let init_state = make_init_state board player_loc floor in 
  let state_with_monsters = add_monsters 
      (create_monsters floor.num_monsters floor.monster_strength) init_state in 
  let state_with_stairs = add_stairs state_with_monsters in
  let state_with_weapons = add_weapons 
      (create_weapons floor.num_weapons floor.monster_strength) 
      state_with_stairs in 
  let state_with_armor = add_armor (floor.num_armor) state_with_weapons in 
  state_with_armor

(** [equipd_weapon state weapon] is the state [state] with the weapon 
    [weapon] equipped and removed from the board. *)
let equip_weapon state (weapon : Weapon.weapon) =
  let weapon_loc = (match weapon.position with 
      | Some (x, y) -> (x, y) 
      | None -> failwith "Weapon must be on the board.") in
  let new_weapon = {weapon with position = None} in
  state.board.(fst weapon_loc).(snd weapon_loc) <- Empty;
  {state with player = {state.player with inventory = Inventory.equip_weapon 
                                              state.player.inventory 
                                              new_weapon}}

(** [pop_weapon loc weapons] is a tuple with the first element being the weapon 
    at location [loc] in weapons and the second being the list [weapons] with 
    that element removed. Requires: There is a weapon in [weapons] at the 
    location [loc]. *)
let rec pop_weapon loc (weapons : Weapon.weapon list) =
  match weapons with 
  | [] -> print_endline "oh no"; failwith "Weapon not in list."
  | h::t when (h.position = Some loc) -> (h, t)
  | h::t -> let x = pop_weapon loc t in 
    (fst x, h :: snd x)

(** [get_name weapon] is the name of the weapon [weapon]. *)
let get_name (weapon : Weapon.weapon) = 
  match weapon.w_type with
  | BattleAxe -> "battle axe"
  | ShortSword -> "short sword"
  | ShortBow -> "short bow"

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
    | Weapon w -> let new_weapon_config = pop_weapon new_pos t.weapons in 
      let new_weapon = fst new_weapon_config in 
      let other_weapons = snd new_weapon_config in
      let equipped_state = equip_weapon {t with weapons = other_weapons} 
          new_weapon in 
      let msg_st = write_msgs equipped_state 
          ["You have picked up a new level " ^ (string_of_int new_weapon.level) 
           ^ " " ^ (get_name new_weapon)  ^ "." ] in
      move_player msg_st new_pos |> inc_turns |> set_energy new_energy
    | Armor a -> let level = a in 
      let armor = Armor.create_armor level in
      let equipped_state = 
        {t with player = {t.player with inventory = equip_armor 
                                            t.player.inventory armor}} in
      let msg_st = write_msgs equipped_state 
          ["You have picked up " ^ (armor.name) 
           ^ "."] in 
      move_player msg_st new_pos |> inc_turns |> set_energy 
        new_energy

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

(** [damage_monster t (x, y) damage] is the state [t] with the monster at
    coordinate [(x, y)] dealth [damage] damage. *)
let damage_monster t (x, y) damage = 
  {t with monsters = List.map (
       fun (m : Monster.monster) -> if m.position = (x, y) then 
           {m with health = m.health - damage} else m) t.monsters}

(** [get_attact_spots weapon dir] is the list of tile locations affected when 
    the weapon [weapon] is used to attack in the direction [dir] and the damage 
    dealth to each of those tiles. *)
let get_attack_spots weapon dir =
  match Weapon.get_type weapon with
  | ShortSword -> Short_sword.Short_Sword.attack weapon dir
  | ShortBow -> Short_bow.Short_Bow.attack weapon dir
  | BattleAxe -> Battleaxe.Battleaxe.attack weapon dir

(** [attack_weapon t weapon dir] is the state [t] with the weapon [weapon]
    having attacked in the direction [dir]. *)
let attack_weapon t weapon dir =
  if t.player.energy < attack_cost
  then 
    write_msgs t 
      ["You do not have enough energy to attack. Try resting."]
  else
    let new_energy = t.player.energy - attack_cost in
    let (pX, pY) = t.player.position in
    let attack_spots = get_attack_spots weapon dir in List.fold_left 
      (fun t (relX, relY, damage) -> 
         damage_monster t (pX + relX, pY + relY) damage) t attack_spots
                                                      |> set_energy new_energy

(** [attack_melee t dir] is the state [t] with a melee attack having been 
    executed in the direction [dir]. *)
let attack_melee t dir = 
  match Inventory.get_melee_weapon t.player.inventory with
  | Some weapon -> attack_weapon t weapon dir
  | None -> t

(** [attack_ranged t dir] is state [t] with a ranged attack having been 
    executed in teh direction [dir]. *)
let attack_ranged t dir = 
  match Inventory.get_ranged_weapon t.player.inventory with
  | Some weapon -> attack_weapon t weapon dir
  | None -> t

(** A list of directions. *)
let d_list = [Up; Down; Left; Right]

(** [display_weapon t w panel (offX, offY)] displays the potential attack 
    zones of the weapon [w] in state [t] and on panel [panel] given the offset 
    [(offX, offY)]. *)
let display_weapon t w panel (offX, offY) =
  let d_coords = match Weapon.get_type w with
    | ShortSword ->
      List.fold_left 
        (fun lst dir -> lst @ (Short_sword.Short_Sword.attack w dir))
        [] d_list
    | ShortBow ->
      List.fold_left (fun lst dir -> lst @ (Short_bow.Short_Bow.attack w dir))
        [] d_list
    | BattleAxe ->
      List.fold_left (fun lst dir -> lst @ (Battleaxe.Battleaxe.attack w dir))
        [] d_list
  in
  let (pX, pY) = t.player.position in
  let prop_coords = List.map (fun (x, y, _) -> (pX + x - offX, pY + y - offY)) 
      d_coords in
  ignore(Ascii_panel.outline_coords prop_coords Graphics.red panel)

(** [display_melee t panel] uses [panel] to display the coordinates that are 
    able to be attacked by the player's melee weapon. *)
let display_melee t panel coord = 
  match Inventory.get_melee_weapon t.player.inventory with
  | Some w -> display_weapon t w panel coord
  | None -> ()

(** [display_ranged t panel] uses [panel] to display the coordinates that are 
    able to be attacked by the player's ranged weapon. *)
let display_ranged t panel coord = 
  match Inventory.get_ranged_weapon t.player.inventory with
  | Some w -> display_weapon t w panel coord
  | None -> ()

let do_display t display panel coord = 
  match display with
  | Help -> ignore(write_help t)
  | PlayerHelp -> ignore(write_player_help t)
  | FightingHelp -> ignore(write_fighting_help t)
  | Inv -> ignore(write_inventory t)
  | Melee -> display_melee t panel coord
  | Ranged -> display_ranged t panel coord

let display_update t display =
  match display with
  | Help -> write_help t
  | PlayerHelp -> write_player_help t
  | FightingHelp -> write_fighting_help t
  | Inv -> write_inventory t
  | _ -> t

(** [do_player_turn t action] is the state of the board after a player's turn
    has been executed on which the player did the action [action]. *)
let do_player_turn t modify =
  (* Attack if enemy present. *)
  match modify with
  | Move direction -> move direction t
  | Break -> break t
  | Rest -> rest t;
  | Melee_Attack dir -> attack_melee t dir
  | Ranged_Attack dir -> attack_ranged t dir

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
  let new_t = 
    {t with 
     monsters = rest_of_monsters;
     messages = Messages.write_msg (m.name ^ "has died. Woohoo.") t.messages;
    } in add_exp new_t m.exp

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