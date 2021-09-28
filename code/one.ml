open Core
open Code.Hashutil

let tuple_of_input a b c = a, b, c
let num_hashes, end_range, file = Scanf.scanf "%d %d %s" tuple_of_input

let hashes =
  generate_hashes ~end_exclusive:end_range ~prime:(larger_prime end_range) num_hashes
;;

let counters = Array.init num_hashes ~f:(fun _ -> Array.init end_range ~f:(fun _ -> 0))

let () =
  In_channel.with_file file ~f:(fun ic ->
      In_channel.iter_lines ic ~f:(fun line ->
          let line = String.strip line in
          let n = Int.of_string line in
          List.iteri hashes ~f:(fun id hash ->
              let output = hash n in
              printf "%d \n" output;
              let counter = Array.get counters id in
              counter.(output) <- counter.(output) + 1)));
  (* Array.set counter output (Array.get counter output + 1))); *)
  Array.iter counters ~f:(Array.iter ~f:(printf "%d "))
;;
