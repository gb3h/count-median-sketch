open Core
open Owl

let tuple_of_input a b c d e = a, b, c, d, e

let num_vals, max_val, distribution, mu, sigma =
  Scanf.scanf "%d %d %s %f %f" tuple_of_input
;;

let generate_gaussian num_elems ~mu ~sigma =
  List.init num_elems ~f:(fun _ ->
      Stats.gaussian_rvs ~mu ~sigma |> Float.round_nearest |> Int.of_float)
;;

let ls =
  match distribution with
  | "gauss" -> generate_gaussian num_vals ~mu ~sigma
  | "uniform" -> List.init num_vals ~f:(fun _ -> Random.int_incl 0 @@ (max_val - 1))
  | "exponential" ->
    List.init num_vals ~f:(fun _ ->
        Stats.exponential_rvs ~lambda:0.5 |> Float.round_nearest |> Int.of_float)
  | _ -> failwith "unexpected distribution"
;;

let () = List.iter ls ~f:(printf "%d\n")
