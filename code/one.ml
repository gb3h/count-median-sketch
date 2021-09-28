open Core

(* Fix a seed for deterministic runs *)
let () = Random.init 5234

let generate_hashes m p k =
  let prime_field_hash a b i = ((a * i) + b) % p % m in
  let ab_list = List.init k ~f:(fun _ -> Random.int_incl 1 (p - 1), Random.int (p - 1)) in
  let hashes = List.map ab_list ~f:(fun (a, b) -> prime_field_hash a b) in
  hashes
;;


(* Read from stdin: Scanf.bscanf Scanf.Scanning.stdin "%d" print_int *)