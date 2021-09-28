open Core

(* Fix a seed for deterministic runs *)
let () = Random.init 5234

let generate_hashes ~end_exclusive ~prime k =
  let prime_field_hash a b i = ((a * i) + b) % prime % end_exclusive in
  let ab_list =
    List.init k ~f:(fun _ -> Random.int_incl 1 (prime - 1), Random.int (prime - 1))
  in
  let hashes = List.map ab_list ~f:(fun (a, b) -> prime_field_hash a b) in
  hashes
;;

let is_prime n =
  let n = abs n in
  let rec is_not_divisor d = d * d > n || (n mod d <> 0 && is_not_divisor (d + 1)) in
  n <> 1 && is_not_divisor 2
;;

let larger_prime n =
  let res = ref (n * 10) in
  while not (is_prime !res) do
    res := !res + 1
  done;
  !res
;;
