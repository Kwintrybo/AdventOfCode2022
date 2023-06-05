open Core

let lines = In_channel.read_lines "day4.txt"

let get_int_at pair i =
  let value = List.nth pair i in
  match value with
  | None -> failwith "Invalid index"
  | Some x -> int_of_string x
;;

let get_pair_values pair =
  let nums = String.split_on_chars pair ~on:[',';'-'] in
  let min1 = get_int_at nums 0 in
  let min2 = get_int_at nums 2 in
  let max1 = get_int_at nums 1 in
  let max2 = get_int_at nums 3 in
  (min1, max1, min2, max2)
;;

let contains_full_range pair = 
  let (min1, max1, min2, max2) = get_pair_values pair in
  (min1 <= min2 && max1 >= max2) || (min2 <= min1 && max2 >= max1)
;;

let overlaps_ranges pair =
  let (min1, max1, min2, max2) = get_pair_values pair in
  min1 <= max2 && max1 >= min2
;;

let count_num_contains pairs =
  let numPairs = List.count pairs ~f:contains_full_range in
  print_endline (string_of_int numPairs)
;;

let () = count_num_contains lines
;;

let count_num_overlapping pairs =
  let numPairs = List.count pairs ~f:overlaps_ranges in
  print_endline (string_of_int numPairs)
;;

let () = count_num_overlapping lines
;;
