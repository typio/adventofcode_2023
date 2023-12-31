let get_card_winners line =
  let content = List.nth (String.split_on_char ':' line) 1 in
  match
    List.map
      (fun section ->
        List.map
          (fun x -> int_of_string x)
          (String.split_on_char ' ' section
          |> List.filter (fun s -> String.length s >= 1)))
      (String.split_on_char '|' content)
  with
  | winning_numbers :: player_numbers :: _ ->
      List.filter_map
        (fun x -> x)
        (List.map
           (fun player_num ->
             List.find_opt
               (fun winning_num -> player_num = winning_num)
               winning_numbers)
           player_numbers)
  | _ -> failwith "Expected two sections seperated by |!"

let rec get_score = function
  | n when n < 0 -> 0
  | 0 -> 1
  | n -> 2 * get_score (n - 1)

let part1 lines =
  List.fold_left ( + ) 0
    (List.map
       (fun line -> get_score (List.length (get_card_winners line) - 1))
       lines)
  |> print_int

let part2 lines =
  let card_counts = Array.make (List.length lines) 1 in
  List.iteri
    (fun i line ->
      let amt = List.length (get_card_winners line) in
      for j = i + 1 to i + amt do
        if j < Array.length card_counts then
          card_counts.(j) <- card_counts.(j) + card_counts.(i)
      done)
    lines;
  Array.fold_left ( + ) 0 card_counts |> print_int

let () =
  try
    let lines = Utils.read_lines "day04.txt" in
    Printf.printf "Timing Part 1\n";
    ignore (Utils.time_fn part1 lines);
    Printf.printf "Timing Part 2\n";
    ignore (Utils.time_fn part2 lines)
  with e ->
    print_endline (Printexc.to_string e);
    raise e
