open OUnit
open Add

let test = "test suite for add" >::: [
  "correct" >:: (fun _ -> assert_equal 3 (add 1 2));
  "wrong"   >:: (fun _ -> assert_equal 0 (add 7 1));
]

let _ = run_test_tt_main test