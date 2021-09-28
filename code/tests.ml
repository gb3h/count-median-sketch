open Assert
open One
let tests : suite =
  [ Test
      ( "Test test"
      , [ "case1", assert_eqf (fun () -> 42) 42
        ; "case2", assert_eqf (fun () -> Int.of_float 5.) 5
        ] )
  ]