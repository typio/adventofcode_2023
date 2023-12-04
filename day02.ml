let total_cubes color_name = 
  match color_name with
  | "red" -> 12
  | "green" -> 13
  | "blue" -> 14
  | _ -> failwith "Invalid color!"

let check_cubes cubes =
  match String.split_on_char ' ' (String.trim cubes) with
  | [amt; color] -> (int_of_string amt) <= (total_cubes color)
  | _ -> failwith "Set doesn't have 2 elements!"
  
let play_game line =
  let parts = String.split_on_char ':' line in
  let (game, sets) = match parts with
  | [first; second] -> (
      int_of_string (List.nth (String.split_on_char ' ' first) 1), 
      String.split_on_char ';' second
    ) 
  | _ -> failwith "Invalid format!"
  in
  if List.for_all 
    (fun set -> List.for_all check_cubes (String.split_on_char ',' set))
  sets 
  then game
  else 0
  
let part1 lines =
  let sum = List.fold_left (fun acc line -> acc + play_game line) 0 lines in
  print_int sum

let color_index color_name = 
  match color_name with
  | "red" -> 0
  | "green" -> 1
  | "blue" -> 2
  | _ -> failwith "Invalid color!"

let set_max_color_count (a, b, c) index n =
  match index with
  | 0 -> (max a n, b, c)
  | 1 -> (a, max b n, c)
  | 2 -> (a, b, max c n)
  | _ -> failwith "Invalid index!" 
  
let part2 lines = 
  let sum = List.fold_left (fun acc line ->
    let parts = String.split_on_char ':' line in
    let sets_of_cubes = String.split_on_char ';' (
      match parts with 
      | [_; sets_string] -> sets_string
      | _ -> failwith "Invalid format!" 
      ) 
    in
    let all_cubes = List.concat_map (fun m -> String.split_on_char ',' m) sets_of_cubes in
    let rec aux counts = function
    | cubes :: remaining_cubes -> aux (
      match String.split_on_char ' ' (String.trim cubes) with
      | [amt; color] -> (set_max_color_count counts (color_index color) (int_of_string amt))
      | _ -> failwith "set_of_cubes doesn't have 2 elements!"
      ) remaining_cubes
    | [] -> match counts with | (a, b, c) -> a * b * c
    in acc + (aux (0, 0, 0) all_cubes)
  ) 0 lines in
  print_int sum

let () =
  try
    let lines = Utils.read_lines "day02.txt" in
    Printf.printf "Timing Part 1\n";
    ignore (Utils.time_fn part1 lines);
    Printf.printf "Timing Part 2\n";
    ignore (Utils.time_fn part2 lines);
  with
  | e -> print_endline (Printexc.to_string e); raise e
