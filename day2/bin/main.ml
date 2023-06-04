open Core

type choice = Rock | Paper | Scissors

let score_round round =
  match round with
  | (Rock, Rock) -> 4
  | (Rock, Paper) -> 8
  | (Rock, Scissors) -> 3
  | (Paper, Rock) -> 1
  | (Paper, Paper) -> 5
  | (Paper, Scissors) -> 9
  | (Scissors, Rock) -> 7
  | (Scissors, Paper) -> 2
  | (Scissors, Scissors) -> 6
;;

let map_to_choice letter =
  match letter with
  | 'A' | 'X' -> Some Rock
  | 'B' | 'Y' -> Some Paper
  | 'C' | 'Z' -> Some Scissors
  | _ -> None
;;

let round_to_choices (round : char list) = List.filter_map round ~f:map_to_choice
;;

let convert_to_tuple (lst : choice list) = match lst with
| x :: y :: _ -> (x, y)
| _ -> failwith "Invalid format for list conversion to tuple"
;;

let rec calc_score rounds score =
  match rounds with
  | [] -> Out_channel.print_endline (string_of_int score)
  | x :: xs -> calc_score xs (score + score_round x)
;;

let rounds = In_channel.read_lines "day2.txt"
|> List.map ~f:String.to_list
|> List.map ~f:round_to_choices
|> List.map ~f:convert_to_tuple in
calc_score rounds 0
;;