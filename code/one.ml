open Core
open Code.Hashutil

let tuple_of_input a b c = a, b, c
let num_hashes, end_range, file = Scanf.scanf "%d %d %s" tuple_of_input

let hashes =
  generate_hashes ~end_exclusive:end_range ~prime:(larger_prime end_range) num_hashes
  |> Array.of_list
;;

let counters = Array.init num_hashes ~f:(fun _ -> Array.init end_range ~f:(fun _ -> 0))

let middle arr =
  let len = Array.length arr in
  let mid = len / 2 in
  if len % 2 = 0
  then (
    let sum = arr.(mid - 1) + arr.(mid) |> Float.of_int in
    sum /. 2.)
  else arr.(mid) |> Float.of_int
;;

let query x =
  let arr = Array.init num_hashes ~f:(fun i -> counters.(i).(hashes.(i) x)) in
  Array.sort arr ~compare:Int.compare;
  middle arr
;;

let print_counters counters =
  Array.iter counters ~f:(fun arr ->
      Array.iter arr ~f:(printf "%d ");
      print_endline "")
;;

let () =
  In_channel.with_file file ~f:(fun ic ->
      In_channel.iter_lines ic ~f:(fun line ->
          let line = String.strip line in
          let n = Int.of_string line in
          Array.iteri hashes ~f:(fun id hash ->
              let output = hash n in
              let counter = Array.get counters id in
              counter.(output) <- counter.(output) + 1)));
  print_counters counters;
  printf "freq 1: %3.f\n" (query 1);
  printf "freq 3: %3.f\n" (query 3)
;;
