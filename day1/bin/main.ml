open Core

let file = "day1.txt"

let safeConverter line =
  match line with
  | "" -> None
  | _ -> Some (int_of_string line)
;;

let read file = List.map (In_channel.read_lines file) ~f:safeConverter

let rec sumElves (lst: int option list) res: int list =
  match lst with
  | [] -> res
  | None :: tl -> sumElves tl (0 :: res)
  | Some cal :: tl -> sumElves tl (
    match res with
    | [] -> [cal]
    | x :: xs -> (x + cal) :: xs
  )
;;

(* Find max in list *)
let maxValue (lst : int list) : int = List.fold lst ~init:Int.min_value ~f:max
;;

let elvesCalories = sumElves (read file) [] in
Out_channel.printf "Max calories: %d\n" (maxValue elvesCalories)