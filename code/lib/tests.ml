open Assert
open Hashutil

let hashes = generate_hashes 5 19 3
let first_hash = List.hd hashes
let second_hash = List.nth hashes 1

let tests : suite =
  [ Test
      ( "Test hash of 0 to 4"
      , [ "case1", assert_eqf (fun () -> first_hash 1) 4
        ; "case2", assert_eqf (fun () -> first_hash 1) 4
        ; "case3", assert_eqf (fun () -> first_hash 98192832) 0
        ; "case4", assert_eqf (fun () -> first_hash 8312) 0
        ; "case5", assert_eqf (fun () -> first_hash 19723) 4
        ; "case6", assert_eqf (fun () -> second_hash 1) 4
        ; "case7", assert_eqf (fun () -> second_hash 1) 4
        ; "case8", assert_eqf (fun () -> second_hash 98192832) 3
        ; "case9", assert_eqf (fun () -> second_hash 8312) 1
        ; "case10", assert_eqf (fun () -> second_hash 19723) 4
        ] )
  ]
;;
