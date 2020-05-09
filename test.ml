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

let tests = [

]

let suite =
  "test suite for our project"  >::: List.flatten [
    tests;
    state_tests;
  ]

let _ = run_test_tt_main suite