#mod_use "utils.ml"

type race = { time : int; distance : int }

let solve_races races =
  Array.fold_left
    (*
    Can create formula instead of looping through!
      d = (t - st) * st with st in [0, t]
      t st - st st = d
      st^2 - t st + d 
      a = 1, b = -t, c = d
      st = (t +- (sqrt t^2 - 4d)) / 2
   *)
      (fun acc race ->
      let ans =
        Float.ceil
          ((float race.time
           +. sqrt (float ((race.time * race.time) - (4 * race.distance))))
          /. 2.0)
        -. Float.floor
             ((float race.time
              -. sqrt (float ((race.time * race.time) - (4 * race.distance))))
             /. 2.0)
        -. 1.
      in
      Float.to_int ans * acc)
    1 races

let part1 lines =
  let read_row i =
    (Utils.split_string ':' lines.(i)).(1)
    |> Utils.split_string ' '
    |> Array.map int_of_string_opt
    |> Utils.array_filter_some
  in

  let times = read_row 0 in
  let distances = read_row 1 in

  Array.map2 (fun t d -> { time = t; distance = d }) times distances
  |> solve_races |> print_int

let part2 lines =
  let read_row i =
    (Utils.split_string ':' lines.(i)).(1)
    |> Utils.split_string ' '
    |> Array.fold_left
         (fun x acc -> if String.equal (String.trim x) "" then acc else x ^ acc)
         ""
    |> int_of_string
  in

  let time = read_row 0 in
  let distance = read_row 1 in

  [| { time; distance } |] |> solve_races |> print_int

let () =
  try
    let lines = Utils.get_lines_arr "day06.txt" in
    Printf.printf "Timing Part 1\n";
    ignore (Utils.time_fn part1 lines);
    Printf.printf "Timing Part 2\n";
    ignore (Utils.time_fn part2 lines)
  with e ->
    print_endline (Printexc.to_string e);
    raise e
