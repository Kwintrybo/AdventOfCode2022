open Core

type choice = Rock | Paper | Scissors | Lose | Draw | Win

let score_round round =
  match round with
  | (Rock, Draw) -> 4
  | (Rock, Win) -> 8
  | (Rock, Lose) -> 3
  | (Paper, Lose) -> 1
  | (Paper, Draw) -> 5
  | (Paper, Win) -> 9
  | (Scissors, Win) -> 7
  | (Scissors, Lose) -> 2
  | (Scissors, Draw) -> 6
  | _ -> failwith "Invalid combination"
;;

let map_to_choice letter =
  match letter with
  | 'A' -> Some Rock
  | 'B' -> Some Paper
  | 'C' -> Some Scissors
  | 'X' -> Some Lose
  | 'Y' -> Some Draw
  | 'Z' -> Some Win
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