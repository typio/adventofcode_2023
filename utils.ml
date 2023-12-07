let read_lines filename =
  let rec aux ic acc =
    match input_line ic with
    | line -> aux ic (line :: acc)
    | exception End_of_file ->
        close_in ic;
        List.rev acc
  in
  aux (open_in filename) []

let get_lines_arr filename = read_lines filename |> Array.of_list
let is_digit c = c >= '0' && c <= '9'
let listify_string s = s |> String.to_seq |> List.of_seq
let explode s = s |> String.to_seq |> Array.of_seq
let stringify_list l = l |> List.to_seq |> String.of_seq
let reverse_string s = stringify_list (List.rev (listify_string s))
let stringify_char c = String.make 1 c

let split_string delimiter str =
  str |> String.split_on_char delimiter |> Array.of_list

let array_filter_map f arr =
  let mapped = Array.map f arr in
  let filtered =
    Array.fold_right
      (fun x acc -> match x with Some v -> v :: acc | None -> acc)
      mapped []
  in
  Array.of_list filtered

let array_filter_some arr = array_filter_map (fun x -> x) arr

let time_fn f x =
  let t = Sys.time () in
  let fx = f x in
  Printf.printf "\nExecution time: %fms\n" ((Sys.time () -. t) *. 1000.0);
  fx
