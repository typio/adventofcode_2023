#mod_use "utils.ml"

module Network = Map.Make (String)

type neighbors = { left : string; right : string }

(* Adj list graph representation *)
let create_network lines =
  Array.sub lines 2 (Array.length lines - 2)
  |> Array.fold_left
       (fun network line ->
         let line_parts = Utils.split_string '=' line in
         let src = line_parts.(0) |> String.trim in
         let left, right =
           (String.sub line_parts.(1) 2 3, String.sub line_parts.(1) 7 3)
         in
         network |> Network.add src { left; right })
       Network.empty

let part1 lines =
  let directions = lines.(0) in
  let network = create_network lines in

  let rec loop node i =
    let next_node =
      match directions.[i mod String.length directions] with
      | 'L' -> (Network.find node network).left
      | 'R' -> (Network.find node network).right
      | _ -> failwith "Invalid direction!"
    in
    if next_node = "ZZZ" then i + 1 else loop next_node (i + 1)
  in

  loop "AAA" 0 |> print_int

let rec gcd a b = match b with 0 -> a | _ -> gcd b (a mod b)
let lcm numbers = Array.fold_left (fun a b -> a * b / gcd a b) 1 numbers

let part2 lines =
  let directions = lines.(0) in
  let network = create_network lines in

  let rec loop nodes first_matches i =
    let next_nodes =
      Array.mapi
        (fun j node ->
          let neighbor =
            match directions.[i mod String.length directions] with
            | 'L' -> (Network.find node network).left
            | 'R' -> (Network.find node network).right
            | _ -> failwith "Invalid direction!"
          in
          if neighbor.[2] = 'Z' && first_matches.(j) = 0 then
            first_matches.(j) <- i + 1;
          neighbor)
        nodes
    in
    if Array.for_all (fun n -> n <> 0) first_matches then first_matches
    else loop next_nodes first_matches (i + 1)
  in

  let initial_nodes =
    Network.filter (fun node _ -> node.[2] == 'A') network
    |> Network.to_list |> Array.of_list
    |> Array.map (fun (node, _) -> node)
  in

  loop initial_nodes (Array.make (Array.length initial_nodes) 0) 0
  |> lcm |> print_int

let () =
  try
    let lines = Utils.get_lines_arr "day08.txt" in
    Printf.printf "Timing Part 1\n";
    ignore (Utils.time_fn part1 lines);
    Printf.printf "Timing Part 2\n";
    ignore (Utils.time_fn part2 lines)
  with e ->
    print_endline (Printexc.to_string e);
    raise e
