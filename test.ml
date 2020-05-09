open OUnit2
open State
open Messages


let tuple_print (w, z) =
  "(" ^ string_of_int w ^ ", " ^ string_of_int z ^ ") "

let state_1 = State.init_level ()
let state_2 = State.init_level ()

let player = State.get_stats state_1

let (x, y) = State.get_player_pos state_1
let pos_up = (x, y + 1)
let pos_down = (x, y - 1)
let pos_left = (x - 1, y)
let pos_right = (x + 1, y)

let state_up = State.do_turn state_1 (Move Up)
let player_up = State.get_player_pos state_up

let state_down = State.do_turn state_1 (Move Down)
let player_down = State.get_player_pos state_down

let state_right = State.do_turn state_1 (Move Right)
let player_right = State.get_player_pos state_right

let state_left = State.do_turn state_1 (Move Left)
let player_left = State.get_player_pos state_left




let state_tests = [
  (* This should almost always, if not always, work. *)
  "Worlds are randomized." >:: (fun _ -> 
      assert_equal false (state_1 = state_2));

  "Move Up works" >:: (fun _ ->
      assert_equal pos_up player_up ~printer:tuple_print);
  "Move Down works" >:: (fun _ ->
      assert_equal pos_down player_down ~printer:tuple_print); 
  "Move Right works" >:: (fun _ ->
      assert_equal pos_right player_right ~printer:tuple_print);
  "Move Left works" >:: (fun _ ->
      assert_equal pos_left player_left ~printer:tuple_print);

  "Player starts with correct health" >:: (fun _ ->
      assert_equal (State.get_stats state_2).health 10 ~printer:string_of_int);

  "Player starts with correct max health" >:: (fun _ ->
      assert_equal (State.get_stats state_2).max_health 
        10 ~printer:string_of_int);

  "Player starts with correct xp" >:: (fun _ ->
      assert_equal (State.get_stats state_2).exp 0 ~printer:string_of_int);

  "Player starts with correct xp" >:: (fun _ ->
      assert_equal (State.get_stats state_2).exp 0 ~printer:string_of_int);

]

let tests = [

]

let suite =
  "test suite for our project"  >::: List.flatten [
    tests;
    state_tests;
  ]

let _ = run_test_tt_main suite