open OUnit2
open State
open Messages
open Graphics

let tuple_print (w, z) =
  "(" ^ string_of_int w ^ ", " ^ string_of_int z ^ ") "

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
      assert_equal (85,38) (get_board_size state_1) ~printer:tuple_print);

  "World size increases correctly after next_level is run" >:: (fun _ -> 
      assert_equal (90,40) (get_board_size state_3) ~printer:tuple_print);
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
]

let monster_tests = [

]

let weapon_tests = [

]

let board_tests = [

]

let inventory_tests = [

]

let main_tests = [

]

let name_tests = [

]

let suite =
  "test suite for our project"  >::: List.flatten [
    state_tests;
    math_tests;
    messages_tests;
    action_tests;
    monster_tests;
    weapon_tests;
    board_tests;
    inventory_tests;
    main_tests;
    name_tests
  ]

let _ = run_test_tt_main suite