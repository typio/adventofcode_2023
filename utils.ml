let read_lines filename = 
  let rec aux ic acc =
  match input_line ic with
    | line -> aux ic (line :: acc)
    | exception End_of_file -> close_in ic; List.rev acc 
  in
  aux (open_in filename) []

let is_digit c = c >= '0' && c <= '9'
let listify_string s = s |> String.to_seq |> List.of_seq
let stringify_list l = l |> List.to_seq |> String.of_seq
let reverse_string s = stringify_list (List.rev (listify_string s))
let stringify_char c = String.make 1 c

let time_fn f x =
  let t = Sys.time() in
  let fx = f x in
  Printf.printf "\nExecution time: %fms\n" ((Sys.time() -. t) *. 1000.0);
  fx
  