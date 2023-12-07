(* #mod_use "utils.ml" *)

type category_map = { src : int; dst : int; rng : int }

let parse_maps lines =
  let accumulated, left_over =
    Array.fold_left
      (fun (acc, part) line ->
        if String.length line > 0 && (Utils.explode line).(0) |> Utils.is_digit
        then
          let nums = Utils.split_string ' ' line |> Array.map int_of_string in
          (acc, { src = nums.(1); dst = nums.(0); rng = nums.(2) } :: part)
        else if part = [] then (acc, part)
        else (acc @ [ Array.of_list (List.rev part) ], []))
      ([], []) lines
  in
  (if left_over = [] then accumulated
   else accumulated @ [ Array.of_list (List.rev left_over) ])
  |> Array.of_list

let map_input (input : int) (maps : category_map array) : int =
  let rec loop i =
    match i with
    | i when i >= Array.length maps -> input
    | _ ->
        let map = maps.(i) in
        if input >= map.src && input < map.src + map.rng then
          map.dst + (input - map.src)
        else loop (i + 1)
  in
  loop 0

let get_seeds lines =
  (Utils.split_string ':' lines.(0)).(1)
  |> Utils.split_string ' '
  |> Array.map int_of_string_opt
  |> Utils.array_filter_some

let part1 lines =
  let all_maps = parse_maps lines in

  get_seeds lines
  |> Array.map (fun seed -> Array.fold_left map_input seed all_maps)
  |> Array.fold_left min max_int
  |> print_int

let part2 lines =
  (* I tried to work with ranges but it was hard *)
  (* And the brute force runs in 3 minutes *)
  let seeds = get_seeds lines in
  let all_maps = parse_maps lines in

  Array.make (Array.length seeds / 2) 0
  |> Array.mapi (fun i _ ->
         Array.make seeds.((i * 2) + 1) 0
         |> Array.mapi (fun j _ -> seeds.(i * 2) + j)
         |> Array.map (fun seed -> Array.fold_left map_input seed all_maps)
         |> Array.fold_left min max_int)
  |> Array.fold_left min max_int
  |> print_int

let () =
  try
    let lines = Utils.get_lines_arr "day05.txt" in
    Printf.printf "Timing Part 1\n";
    ignore (Utils.time_fn part1 lines);
    Printf.printf "Timing Part 2\n";
    ignore (Utils.time_fn part2 lines)
  with e ->
    print_endline (Printexc.to_string e);
    raise e
