open OUnit
open Lab03

let tests = "test suite for lab03" >::: [
  "empty" >:: (fun _ -> assert_equal 1 (prod []));
  "six" >:: (fun _ -> assert_equal 6 (prod [1;2;3]));
  "descending" >:: (fun _ -> assert_equal [4;3;2;1] (dsort [3;4;1;2]));
  "two_eq" >:: (fun _ -> assert_equal true (two_eq [1;1;2]));
]

let _ = run_test_tt_main tests