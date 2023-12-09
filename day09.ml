#mod_use "utils.ml"

let extrapolate (forwards : bool) line : int =
  let numbers = Utils.split_string ' ' line |> Array.map int_of_string in

  let rec expand_out numbers expansions =
    let _, next_expansion =
      Array.fold_left
        (fun (last, differences) x ->
          ( x,
            if last <> -max_int then Array.append differences [| x - last |]
            else differences ))
        (-max_int, [||]) numbers
    in

    if Array.for_all (( = ) 0) next_expansion then expansions
    else
      expand_out next_expansion (Array.append [| next_expansion |] expansions)
  in

  let expansions = expand_out numbers [| numbers |] in

  if forwards then
    Array.fold_left
      (fun acc expansion -> expansion.(Array.length expansion - 1) + acc)
      0 expansions
  else Array.fold_left (fun acc expansion -> expansion.(0) - acc) 0 expansions

let part1 lines =
  lines |> Array.map (extrapolate true) |> Array.fold_left ( + ) 0 |> print_int

let part2 lines =
  lines |> Array.map (extrapolate false) |> Array.fold_left ( + ) 0 |> print_int

let () =
  try
    let lines = Utils.get_lines_arr "day09.txt" in
    Printf.printf "Timing Part 1\n";
    ignore (Utils.time_fn part1 lines);
    Printf.printf "Timing Part 2\n";
    ignore (Utils.time_fn part2 lines)
  with e ->
    print_endline (Printexc.to_string e);
    raise e
