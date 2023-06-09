open Core

module Chars = Set.Make(Char)

let lines = In_channel.read_lines "day3.txt"

let split_string str = List.split_n (String.to_list str) (String.length str / 2)
;;

let find_common (a, b) =
  Set.to_list (Set.inter (Chars.of_list a) (Chars.of_list b))
;;

let rec get_all_common lines =
  match lines with
  | [] -> []
  | line :: rest -> find_common (split_string line) :: get_all_common rest
;;

let flatten lst =
  let rec helper acc = function
    | [] -> acc
    | [] :: t -> helper acc t
    | (x :: y) :: t -> helper (x :: acc) (y :: t) in
  helper [] lst
;;

let char_to_priority c =
  if (Char.is_lowercase c) then
    (Char.to_int c) - (Char.to_int 'a') + 1
  else
    (Char.to_int c) - (Char.to_int 'A') + 27
;;

let () = List.map (flatten (get_all_common lines)) ~f:char_to_priority
|> List.fold ~init:0 ~f:Int.(+)
|> printf "%d\n"
;;

let get_badge x y z =
  let a = String.to_list x in
  let b = String.to_list y in
  let c = String.to_list z in
  find_common (find_common (a, b), c)
;;

let rec get_all_badges lines =
  match lines with
  | [] -> []
  | x :: y :: z :: t -> get_badge x y z :: get_all_badges t
  | _ -> failwith "Invalid number of groups"
;;

let () = List.map (flatten (get_all_badges lines)) ~f:char_to_priority
|> List.fold ~init:0 ~f:Int.(+)
|> printf "%d\n"
