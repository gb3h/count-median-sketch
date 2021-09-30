open Core
open Code.Hashutil
open Owl
open Owl_plplot

let tuple_of_input a b c d e = a, b, c, d, e

let num_hashes, hash_range, max_value, file_prefix, output =
  Scanf.scanf "%d %d %d %s %s" tuple_of_input
;;

let middle arr =
  let len = Array.length arr in
  let mid = len / 2 in
  if len % 2 = 0
  then (
    let sum = arr.(mid - 1) + arr.(mid) |> Float.of_int in
    sum /. 2.)
  else arr.(mid) |> Float.of_int
;;

let print_counters counters =
  Array.iter counters ~f:(fun arr ->
      Array.iter arr ~f:(printf "%d ");
      print_endline "")
;;

let run_on_file file =
  let hashes = generate_hashes ~end_exclusive:hash_range num_hashes |> Array.of_list in
  let real_counts = Array.init max_value ~f:(fun _ -> 0) in
  let counters =
    Array.init num_hashes ~f:(fun _ -> Array.init hash_range ~f:(fun _ -> 0))
  in
  let min_counter x =
    let arr = Array.init num_hashes ~f:(fun i -> counters.(i).(hashes.(i) x)) in
    Array.sort arr ~compare:Int.compare;
    (* middle arr *)
    arr.(0) |> Int.to_float
  in
  let algorithm_one_query x =
    let arr = Array.init num_hashes ~f:(fun i -> counters.(i).(hashes.(i) x)) in
    Array.sort arr ~compare:Int.compare;
    middle arr
  in
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
  in
  In_channel.with_file file ~f:(fun ic ->
      In_channel.iter_lines ic ~f:(fun line ->
          let line = String.strip line in
          let n = Int.of_string line in
          real_counts.(n) <- real_counts.(n) + 1;
          Array.iteri hashes ~f:(fun id hash ->
              let output = hash n in
              let counter = counters.(id) in
              counter.(output) <- counter.(output) + 1)));
  real_counts, min_counter, algorithm_one_query, algorithm_two_query
;;

let get_query_data file queries =
  let real_counts, min_counter, algorithm_one_query, algorithm_two_query =
    run_on_file file_prefix
  in
  List.map queries ~f:(fun elem ->
      let actual_count = real_counts.(elem) |> Float.of_int in
      let min_count = min_counter elem in
      let algo_one_res = algorithm_one_query elem in
      let algo_two_res = algorithm_two_query elem in
      elem, actual_count, min_count, algo_one_res, algo_two_res)
;;

let print_query_data (elem, actual_count, min_count, r1, r2) =
  printf "%d ACT:%f MC:%f MED:%f EST:%f\n" elem actual_count min_count r1 r2
;;

let res_ls =
  let stride = max_value / 10 in
  get_query_data file_prefix (List.range ~stride 0 max_value)
;;

let plot res_ls =
  (* Initialise matrix for plotting *)
  let len = List.length res_ls in
  let actual = Mat.empty len 1 in
  let min = Mat.empty len 1 in
  let res_of_1 = Mat.empty len 1 in
  let res_of_2 = Mat.empty len 1 in
  let h = Plot.create ~m:2 ~n:2 output in
  (* Add results to [Plot.matrices] *)
  List.iteri res_ls (fun i e ->
      let elem, actual_count, min_count, res_one, res_two = e in
      print_query_data e;
      Mat.set actual i 0 actual_count;
      Mat.set min i 0 min_count;
      Mat.set res_of_1 i 0 res_one;
      Mat.set res_of_2 i 0 res_two);
  (* Plot each matrix *)
  Plot.subplot h 0 0;
  Plot.set_title h "actual";
  Plot.bar ~h actual;
  Plot.set_xlabel h "value";
  Plot.set_ylabel h "count";
  Plot.subplot h 0 1;
  Plot.set_title h "min count sketch";
  Plot.bar ~h min;
  Plot.set_xlabel h "value";
  Plot.set_ylabel h "count";
  Plot.subplot h 1 0;
  Plot.set_title h "algorithm 1";
  Plot.bar ~h res_of_1;
  Plot.set_xlabel h "value";
  Plot.set_ylabel h "count";
  Plot.subplot h 1 1;
  Plot.set_title h "algorithm 2";
  Plot.bar ~h res_of_2;
  Plot.set_xlabel h "value";
  Plot.set_ylabel h "count";
  Plot.output h
;;

let () = plot res_ls
