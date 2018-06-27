open OUnit2
open Lab05

let tests = "test suite for lab05" >::: [
    "list_max" >:: (fun _ -> assert_raises (Failure "list_max") (fun () -> list_max []));
]

let _ = run_test_tt_main tests;;