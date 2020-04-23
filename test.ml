open OUnit2
open Yojson.Basic

let tests = [

]

let suite =
  "test suite for A-team Roguelike!"  >::: List.flatten [
    tests;
  ]

let _ = run_test_tt_main suite