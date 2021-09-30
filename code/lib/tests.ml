open Assert
open Hashutil

let hashes = generate_hashes 8 3
let first_hash = List.hd hashes
let second_hash = List.nth hashes 1

let tests : suite =
  [ Test
      ( "Test hash of 0 to 4"
      , [ "case1", assert_eqf (fun () -> first_hash 1) 3
        ; "case2", assert_eqf (fun () -> first_hash 1) 3
        ; "case3", assert_eqf (fun () -> first_hash 98192832) 7
        ; "case4", assert_eqf (fun () -> first_hash 8312) 1
        ; "case5", assert_eqf (fun () -> first_hash 19723) 6
        ; "case6", assert_eqf (fun () -> first_hash 12398) 2
        ; "case7", assert_eqf (fun () -> first_hash 58613) 2
        ; "case8", assert_eqf (fun () -> first_hash 751) 4
        ; "case9", assert_eqf (fun () -> first_hash 20) 1
        ; "case10", assert_eqf (fun () -> first_hash 18) 3
        ] )
  ]
;;
