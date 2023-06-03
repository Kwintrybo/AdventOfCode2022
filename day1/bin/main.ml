open Core

let file = "day1.txt"
;;

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

let elvesCalories = sumElves (read file) []
;;

Out_channel.printf "Max calories: %d\n" (maxValue elvesCalories)

(* Copied code *)
let rec max3 elvesCalories (m1, m2, m3) =
  match elvesCalories with
  | [] -> (m1, m2, m3)
  | hd :: tl -> max3 tl (
    match (m1, m2, m3) with
    | m1, m2, _ when hd > m1 -> (hd, m1, m2)
    | m1, m2, _ when hd > m2 -> (m1, hd, m2)
    | m1, m2, m3 when hd > m3 -> (m1, m2, hd)
    | _ -> (m1, m2, m3)
  )
;;

let (m1, m2, m3) = max3 elvesCalories (Int.min_value, Int.min_value, Int.min_value) in
Out_channel.printf "Max 3 values: %d, %d, %d\nTotal sum = %d\n" m1 m2 m3 (m1 + m2 + m3)