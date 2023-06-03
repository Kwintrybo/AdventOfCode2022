let file = "day1.txt"

let process line =
  print_endline line;
;;

let read_lines file process =
  let ic = open_in file in
  let rec read_line () =
    let line = try input_line ic with End_of_file -> exit 0
    in
      process line;
      read_line ();
  in read_line ()
;;

let () = read_lines file process;;