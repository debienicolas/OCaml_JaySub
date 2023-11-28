open Alcotest



let test () = 
  check int "test"  1 1

let () =
  run "test" [
    "test", [
      test_case "test" `Quick test;
    ];
  ]