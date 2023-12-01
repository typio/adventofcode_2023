let read_lines filename = 
  let rec aux ic acc =
  match input_line ic with
    | line -> aux ic (line :: acc)
    | exception End_of_file -> close_in ic; List.rev acc 
  in
  aux (open_in filename) []