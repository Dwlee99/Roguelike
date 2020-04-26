open OUnit2


let tuple_print (w, z) =
  "(" ^ string_of_int w ^ ", " ^ string_of_int z ^ ") "

let state_1 = State.init_game 100 100
let state_2 = State.init_game 100 100

let player = State.get_player state_1

let (x, y) = player.position
let pos_up = (x, y + 1)
let pos_down = (x, y - 1)
let pos_left = (x - 1, y)
let pos_right = (x + 1, y)

let state_up = State.update state_1 (Move Up)
let player_up = State.get_player state_up

let state_down = State.update state_1 (Move Down)
let player_down = State.get_player state_down

let state_right = State.update state_1 (Move Right)
let player_right = State.get_player state_right

let state_left = State.update state_1 (Move Left)
let player_left = State.get_player state_left




let state_tests = [
  (* This should almost always, if not always, work. *)
  "Worlds are randomized." >:: (fun _ -> 
      assert_equal false (state_1 = state_2));
  "Move Up works" >:: (fun _ ->
      assert_equal pos_up player_up.position ~printer:tuple_print);
  "Move Down works" >:: (fun _ ->
      assert_equal pos_down player_down.position ~printer:tuple_print); 
  "Move Right works" >:: (fun _ ->
      assert_equal pos_right player_right.position ~printer:tuple_print);
  "Move Left works" >:: (fun _ ->
      assert_equal pos_left player_left.position ~printer:tuple_print);

]

let tests = [

]

let suite =
  "test suite for our project"  >::: List.flatten [
    tests;
    state_tests;
  ]

let _ = run_test_tt_main suite