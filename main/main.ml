open Assert
open Arg
open Code

exception Ran_tests

let worklist = ref []

let suite = ref (Tests.tests)

let exec_tests () =
  let o = run_suite !suite in
  Printf.printf "%s\n" (outcome_to_string o) ;
  raise Ran_tests


let do_one_file fn =
  let _ = Printf.printf "Processing: %s\n" fn in
  ()


(* Use the --test option to run unit tests and the quit the program. *)
let argspec =
  [ ("--test", Unit exec_tests, "run the test suite, ignoring other inputs") ]
  

let _ =
  try
    Arg.parse
      argspec
      (fun f -> worklist := f :: !worklist)
      "Main test harness \n" ;
    match !worklist with
    | [] ->
        print_endline "* Nothing to do"
    | _ ->
        List.iter do_one_file !worklist
  with
  | Ran_tests ->
      ()
