open Core
open Code.Hashutil

let tuple_of_input a b c d e = a, b, c, d, e

let num_hashes, end_range, max_value, file, x =
  Scanf.scanf "%d %d %d %s %d" tuple_of_input
;;

let hashes = generate_hashes ~end_exclusive:end_range num_hashes |> Array.of_list
let real_counts = Array.init max_value ~f:(fun _ -> 0)
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

let algorithm_one_query x =
  let arr = Array.init num_hashes ~f:(fun i -> counters.(i).(hashes.(i) x)) in
  Array.sort arr ~compare:Int.compare;
  middle arr
;;

let algorithm_two_query x =
  let estimate x i =
    let jay = hashes.(i) x in
    let neighbour =
      if jay % 2 = 0 then counters.(i).(jay + 1) else counters.(i).(jay - 1)
    in
    let value = counters.(i).(jay) in
    value - neighbour
  in
  let arr = Array.init num_hashes ~f:(fun i -> estimate x i) in
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
          real_counts.(n) <- real_counts.(n) + 1;
          Array.iteri hashes ~f:(fun id hash ->
              let output = hash n in
              let counter = Array.get counters id in
              counter.(output) <- counter.(output) + 1)));
  let actual_count = real_counts.(x) in
  let algo_one_res = algorithm_one_query x |> Float.round_nearest |> Int.of_float in
  let algo_two_res = algorithm_two_query x |> Float.round_nearest |> Int.of_float in
  printf "x: %d\n" x;
  printf "real freq: %d\n" actual_count;
  printf "algo 1 query: %d\n" algo_one_res;
  printf "algo 1 diff: %d\n" (abs (actual_count - algo_one_res));
  printf "algo 2 query: %d\n" algo_two_res;
  printf "algo 2 diff: %d\n" (abs @@ (actual_count - algo_two_res))
;;
