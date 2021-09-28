open Assert
open One

let hashes = generate_hashes 5 19 3
let first_hash = List.hd hashes

let tests : suite =
  [ Test
      ( "Test hash of 0 to 4"
      , [ "case1", assert_eqf (fun () -> first_hash 1) 4
        ; "case2", assert_eqf (fun () -> first_hash 1) 4
        ; "case3", assert_eqf (fun () -> first_hash 98192832) 0
        ; "case4", assert_eqf (fun () -> first_hash 8312) 0
        ; "case5", assert_eqf (fun () -> first_hash 19723) 4
        ] )
  ]
;;
