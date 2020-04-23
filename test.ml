open OUnit2

let tests = [

]

let suite =
  "test suite for our project"  >::: List.flatten [
    tests;
  ]

let _ = run_test_tt_main suite