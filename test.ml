open OUnit2


let tuple_print (w, z) =
  "(" ^ string_of_int w ^ ", " ^ string_of_int z ^ ") "

let state_1 = State.init_game 100 100
let state_2 = State.init_game 100 100
let state_3 = State.init_game 100 100
let state_4 = State.init_game 100 100

let player_1 = State.get_player state_1
let player_2 = State.get_player state_2
let player_3 = State.get_player state_3
let player_4 = State.get_player state_4

let pos_up (x, y) = (x, y + 1)
let pos_down (x, y) = (x, y - 1)
let pos_left (x, y) = (x - 1, y)
let pos_right (x, y) = (x + 1, y)

let one_up = pos_up player_1.position
let two_down = pos_down player_2.position
let three_right = pos_right player_3.position
let four_left = pos_left player_4.position

let state_up = State.update state_1 (Move Up)
let player_up = State.get_player state_up

let state_down = State.update state_2 (Move Down)
let player_down = State.get_player state_down

let state_right = State.update state_3 (Move Right)
let player_right = State.get_player state_right

let state_left = State.update state_4 (Move Left)
let player_left = State.get_player state_left




let state_tests = [
  (* This should almost always, if not always, work. *)
  "Worlds are randomized." >:: (fun _ -> 
      assert_equal false (state_1 = state_2));
  "Move Up works" >:: (fun _ ->
      assert_equal one_up player_up.position ~printer:tuple_print);
  "Move Down works" >:: (fun _ ->
      assert_equal two_down player_down.position ~printer:tuple_print); 
  "Move Right works" >:: (fun _ ->
      assert_equal three_right player_right.position ~printer:tuple_print);
  "Move Left works" >:: (fun _ ->
      assert_equal four_left player_left.position ~printer:tuple_print);

]

let tests = [

]

let suite =
  "test suite for our project"  >::: List.flatten [
    tests;
    state_tests;
  ]

let _ = run_test_tt_main suite