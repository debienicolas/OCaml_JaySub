open Jaysub.Store
open Alcotest


let input = ["x";"y"]
let expected_identifiers = ["x";"y"]
let expected_values = [0;0]

let actual_store =  (create input)


let store_1:store = {ids=["x";"y"]; values=[2;3]}
let removed_store_1:store =  (remove_pair store_1 "y")

let store_2:store = {ids=["x";"y";"z"]; values=[2;3;4]}
let removed_store_2:store =  (remove_pair store_2 "y")

let store_3:store = (set_value store_1 "x" 5)

let test_create_identifiers () = 
  check (list string) "create" expected_identifiers (get_ids actual_store)
let test_create_values () =
  check (list int) "create" expected_values (get_values actual_store)

let test_get_value_1 () = 
  check int "get_value" 2 (get_value store_1 "x")
let test_get_value_2 () = 
  check int "get_value" 3 (get_value store_1 "y")

let test_remove_pair_id () =
  check (list string) "remove_pair_id" ["x"] (get_ids removed_store_1)

let test_remove_pair_value () =
  check (list int) "remove_pair_value" [2] (get_values removed_store_1)

let test_remove_pair_id_2 () =
  check (list string) "remove_pair_id" ["x";"z"] (get_ids removed_store_2)
let test_remove_pair_value_2 () =
  check (list int) "remove_pair_value" [2;4] (get_values removed_store_2)

let test_set_value () =
  check int "set_value" 5 (get_value store_3 "x")

let () =
  run "Store tests" [
    "store", [
      test_case "create identifiers" `Quick test_create_identifiers;
      test_case "create values" `Quick test_create_values;
      test_case "get value" `Quick test_get_value_1;
      test_case "get value" `Quick test_get_value_2;
      test_case "remove pair id" `Quick test_remove_pair_id;
      test_case "remove pair value" `Quick test_remove_pair_value;
      test_case "remove pair id 2" `Quick test_remove_pair_id_2;
      test_case "remove pair value 2" `Quick test_remove_pair_value_2;
      test_case "set value" `Quick test_set_value;
    ];
  ]