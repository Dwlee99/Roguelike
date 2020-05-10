open OUnit2
open State
open Messages
open Graphics

let tuple_print (w, z) =
  "(" ^ string_of_int w ^ ", " ^ string_of_int z ^ ") "

let cleared_board = Board.empty_board 100 100

let state_1 = State.init_level ()
let state_2 = State.init_level ()

let state_3 = State.next_level state_1

let player = State.get_stats state_1


let state_tests = [
  (* These tests ensure the correct state of the player in a new game *)
  "Player starts with correct health" >:: (fun _ ->
      assert_equal (State.get_stats state_2).health 10 ~printer:string_of_int);

  "Player starts with correct max health" >:: (fun _ ->
      assert_equal (State.get_stats state_2).max_health 
        10 ~printer:string_of_int);

  "Player starts with correct xp" >:: (fun _ ->
      assert_equal (State.get_stats state_2).exp 0 ~printer:string_of_int);

  "Player starts with correct max xp" >:: (fun _ ->
      assert_equal (State.get_stats state_2).max_exp 9 ~printer:string_of_int);

  "Player starts with correct energy" >:: (fun _ ->
      assert_equal (State.get_stats state_2).energy 
        200 ~printer:string_of_int);

  "Player starts with correct max energy" >:: (fun _ ->
      assert_equal (State.get_stats state_2).max_energy 
        200 ~printer:string_of_int);

  "Player starts with correct level" >:: (fun _ ->
      assert_equal (State.get_stats state_2).level 
        1 ~printer:string_of_int);

  "Player starts with correct turns played" >:: (fun _ ->
      assert_equal (State.get_stats state_2).turns_played 
        0 ~printer:string_of_int);

  "Player starts at the correct floor" >:: (fun _ ->
      assert_equal (State.get_stats state_2).floor 
        1 ~printer:string_of_int);

  (* These tests deal with map generation *)

  (* If this next test fails, it was a very lucky coincedence! s_1 and s_2
     are supposedly randomized! *)
  "Worlds are randomized." >:: (fun _ -> 
      assert_equal false (state_1 = state_2));

  "World size is initially correct." >:: (fun _ -> 
      assert_equal (85,38) (State.get_board_size state_1) ~printer:tuple_print);

  "World size increases correctly after next_level is run" >:: (fun _ -> 
      assert_equal (90,40) (State.get_board_size state_3) ~printer:tuple_print);
]

let math_tests = [
  "Up one test" >:: 
  (fun _ -> assert_equal (5, 3) (Math.up_one (5, 2)));

  "Down one test" >:: 
  (fun _ -> assert_equal (-5, -4) (Math.down_one (-5, -3)));

  "Right one test" >::
  (fun _ -> assert_equal (7, -5) (Math.right_one (6, -5)));

  "Left one test" >::
  (fun _ -> assert_equal (3, 2) (Math.left_one (4, 2)));

  "Square test" >::
  (fun _ -> assert_equal 16 (Math.square 4));

  "Distance squared test" >::
  (fun _ -> assert_equal 25 (Math.distance_sq (2, 1) (5, 5)))
]

let messages_tests = [
  "Writing a message" >::
  (fun _ -> assert_equal "hello" 
      (let init_msgs = ["Playing game."; "Game started."] in 
       List.nth (Messages.write_msg "hello" init_msgs) 0
      ));

  "Writing multiple messages" >::
  (fun _ -> assert_equal ("hello 1", "hello 2")
      (let init_msgs = ["Playing game."; "Game started."] in 
       let added_msgs = ["hello 1"; "hello 2"] in 
       let new_msgs = Messages.write_msgs added_msgs init_msgs in
       (List.nth new_msgs 1, List.nth new_msgs 0)
      ));
]

let action_tests = [
  "Parse up" >:: (fun _ ->
      assert_equal (Action.Modify (Move Up)) (Action.parse 'i'));

  "Parse down" >:: (fun _ ->
      assert_equal (Action.Modify (Move Down)) (Action.parse 'k'));

  "Parse left" >:: (fun _ ->
      assert_equal (Action.Modify (Move Left)) (Action.parse 'j'));

  "Parse right" >:: (fun _ ->
      assert_equal (Action.Modify (Move Right)) (Action.parse 'l'));

  "Parse break" >:: (fun _ ->
      assert_equal (Action.Modify Break) (Action.parse 'b'));

  "Parse help" >:: (fun _ ->
      assert_equal (Action.Display Help) (Action.parse 'h'));

  "Parse player help" >:: (fun _ ->
      assert_equal (Action.Display PlayerHelp) (Action.parse 'p'));

  "Parse combat help" >:: (fun _ ->
      assert_equal (Action.Display FightingHelp) (Action.parse 'f'));

  "Parse quit" >:: (fun _ ->
      assert_equal (Action.Quit) (Action.parse 'q'));

  "Parse ranged attack" >:: (fun _ ->
      assert_equal (Action.Display Ranged) (Action.parse 'r'));

  "Parse melee attack" >:: (fun _ ->
      assert_equal (Action.Display Melee) (Action.parse 'a'));

  "Parse inventory" >:: (fun _ ->
      assert_equal (Action.Display Inv) (Action.parse 'e'));

  "Parse melee attack with direction" >:: (fun _ -> 
      assert_equal (Action.Modify (Melee_Attack Up)) 
        (Action.parse_two 'a' 'i'));

  "Parse melee attack with direction2" >:: (fun _ -> 
      assert_equal (Action.Modify (Melee_Attack Left)) 
        (Action.parse_two 'a' 'j'));

  "Parse ranged attack with direction" >:: (fun _ -> 
      assert_equal (Action.Modify (Ranged_Attack Up)) 
        (Action.parse_two 'r' 'i'));

  "Parse ranged attack with direction2" >:: (fun _ -> 
      assert_equal (Action.Modify (Ranged_Attack Left)) 
        (Action.parse_two 'r' 'j'));
]

let test_monster : Monster.monster = {
  name = "test_monster";
  position = (10, 10);
  m_type = Board.Swordsman;
  health = 5;
  max_health = 10;
  damage = 4;
  exp = 0;
  action_queue = [];
  roaming_target = ref (11,10)
}

let monster_tests = [
  "get monster type test" >:: (fun _ -> 
      assert_equal (Board.Swordsman) (Monster.get_type test_monster));

  "get roaming direction test" >:: (fun _ -> 
      assert_equal (Action.Right) 
        (Monster.get_roam_direction test_monster cleared_board 10 ));
]


let sword : Weapon.weapon = {
  name = "Excalibur";
  position = None;
  w_type = Board.ShortSword;
  damage = 9000;
  atk_type = Weapon.Melee;
  level = 100;
}

let bow : Weapon.weapon = {
  name = "Fafnir's Bane";
  position = None;
  w_type = Board.ShortBow;
  damage = 15;
  atk_type = Weapon.Ranged;
  level = 5;
}

let weapon_tests = [
  "Get weapon type for melee" >:: (fun _ -> 
      assert_equal (Board.ShortSword) (Weapon.get_type sword));

  "Get weapon type for Ranged" >:: (fun _ -> 
      assert_equal (Board.ShortBow) (Weapon.get_type bow));
]

let board_tests = [
  "set tile and get tile" >:: (fun _ -> 
      let coords = (20, 73) in
      let board = cleared_board in 
      Board.set_tile board coords Stairs;
      assert_equal (Board.Stairs) (Board.get_tile board coords));

  "in bounds - false" >:: (fun _ ->
      assert_equal false (Board.in_bound cleared_board (100, 70)));

  "in bounds - true" >:: (fun _ ->
      assert_equal true (Board.in_bound cleared_board (99, 70)));

  "direction_to test 1" >:: (fun _ ->
      assert_equal (Some Action.Left) 
        (Board.direction_to cleared_board (5, 5) (2, 5) 10));

  "direction_to test 2" >:: (fun _ -> 
      let direction = (Board.direction_to cleared_board (5, 5) (7, 7) 10) in
      assert_equal true (((Some Action.Right) = direction) || 
                         ((Some Action.Up) = direction)));

  "direction_to test 3" >:: (fun _ -> 
      let board = cleared_board in 
      Board.set_tile board (8, 10) (Board.Wall true); 
      let direction = (Board.direction_to board (8, 9) (8, 11) 10) in 
      assert_equal true (((Some Action.Right) = direction) || 
                         ((Some Action.Left) = direction)));

  "direction_to test 4" >:: (fun _ -> 
      assert_equal None (Board.direction_to cleared_board (1, 1) (20, 20) 10));

]

let inventory_tests = [
  "init inventory test 1" >:: (fun _ ->
      assert_equal "Short Sword" (Inventory.get_melee_name Inventory.init_inv));

  "init inventory test 2" >:: (fun _ ->
      assert_equal "None" (Inventory.get_ranged_name Inventory.init_inv));

  "init inventory test 3" >:: (fun _ ->
      assert_equal "None" (Inventory.get_armor_name Inventory.init_inv));

  "equip armor test 1" >:: (fun _ ->
      let armor = Armor.create_armor 4 in
      let inv = Inventory.equip_armor Inventory.init_inv armor in
      assert_equal "level 4 armor" (Inventory.get_armor_name inv));

  "equip armor test 2" >:: (fun _ ->
      let armor = Armor.create_armor 4 in
      let inv = Inventory.equip_armor Inventory.init_inv armor in
      assert_equal 4 (Inventory.get_armor_protection inv));

  "equip weapon test 1" >:: (fun _ ->
      let weapon = Battleaxe.Battleaxe.create_weapon 2 in
      let inv = Inventory.equip_weapon Inventory.init_inv weapon in
      assert_equal "Battleaxe" (Inventory.get_melee_name inv));

  "equip weapon test 2" >:: (fun _ ->
      let weapon = Battleaxe.Battleaxe.create_weapon 2 in
      let inv = Inventory.equip_weapon Inventory.init_inv weapon in
      assert_equal (Some weapon) (Inventory.get_melee_weapon inv));
]

let name_tests = [
  "make sure random name isn't empty" >:: (fun _ -> 
      assert_equal true ((String.length (Name.random_name ())) > 0))
]

let suite =
  "test suite for our project"  >::: List.flatten [
    state_tests;
    math_tests;
    messages_tests;
    action_tests;
    monster_tests;
    board_tests;
    inventory_tests;
    name_tests;
    weapon_tests
  ]

let _ = run_test_tt_main suite