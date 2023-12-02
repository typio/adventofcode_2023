let read_lines filename = 
  let rec aux ic acc =
  match input_line ic with
    | line -> aux ic (line :: acc)
    | exception End_of_file -> close_in ic; List.rev acc 
  in
  aux (open_in filename) []

let time_fn f x =
  let t = Sys.time() in
  let fx = f x in
  Printf.printf "\nExecution time: %fms\n" ((Sys.time() -. t) *. 1000.0);
  fx
  