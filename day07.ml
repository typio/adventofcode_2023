#mod_use "utils.ml"

type hand = { cards : string; bid : int }

let get_label_value (joker_mode : bool) = function
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> if joker_mode then 1 else 11
  | 'T' -> 10
  | n -> int_of_string (Char.escaped n)

type maxes = { gold : int; silver : int }

let get_cards_score (j_value : int) (cards : string) : int =
  let card_amts = Array.make 14 0 in
  Utils.explode cards
  |> Array.iteri (fun i c ->
         let label_value =
           (if c = 'J' then j_value else get_label_value false c) - 1
         in
         card_amts.(label_value) <- card_amts.(label_value) + 1);
  match
    Array.fold_left
      (fun maxes card_amt ->
        if card_amt > maxes.gold then { gold = card_amt; silver = maxes.gold }
        else if card_amt > maxes.silver then
          { gold = maxes.gold; silver = card_amt }
        else maxes)
      { gold = 0; silver = 0 } card_amts
  with
  | { gold; silver } when gold = 5 -> 7
  | { gold; silver } when gold = 4 -> 6
  | { gold; silver } when gold = 3 && silver = 2 -> 5
  | { gold; silver } when gold = 3 && silver = 1 -> 4
  | { gold; silver } when gold = 2 && silver = 2 -> 3
  | { gold; silver } when gold = 2 && silver = 1 -> 2
  | _ -> 1

let compare_hands (joker_mode : bool) (hand_a : hand) (hand_b : hand) : int =
  let cards_a = hand_a.cards in
  let cards_b = hand_b.cards in
  let get_score joker_mode cards =
    if joker_mode then
      Array.fold_left max (-max_int)
        (Array.map (get_label_value joker_mode) (Utils.explode cards)
        |> Array.mapi (fun i e -> get_cards_score e cards))
    else get_cards_score 11 cards
  in
  let a_score = get_score joker_mode cards_a in
  let b_score = get_score joker_mode cards_b in
  if a_score > b_score then 1
  else if b_score > a_score then -1
  else
    match
      Array.fold_right
        (fun _ (i, acc) ->
          if acc = 0 then
            let a_label_value = get_label_value joker_mode cards_a.[i] in
            let b_label_value = get_label_value joker_mode cards_b.[i] in
            if a_label_value > b_label_value then (i + 1, 1)
            else if b_label_value > a_label_value then (i + 1, -1)
            else (i + 1, 0)
          else (i + 1, acc))
        (Array.make 5 0) (0, 0)
    with
    | _, acc -> acc

let get_hands lines =
  Array.map
    (fun line ->
      let line_parts = Utils.split_string ' ' line in
      { cards = line_parts.(0); bid = int_of_string line_parts.(1) })
    lines

let solve lines joker_mode =
  let hands = get_hands lines in
  Array.sort (fun a b -> compare_hands joker_mode a b) hands;
  Array.mapi (fun i hand -> (i + 1) * hand.bid) hands
  |> Array.fold_left ( + ) 0 |> print_int

let part1 lines = solve lines false
let part2 lines = solve lines true

let () =
  try
    let lines = Utils.get_lines_arr "day07.txt" in
    Printf.printf "Timing Part 1\n";
    ignore (Utils.time_fn part1 lines);
    Printf.printf "Timing Part 2\n";
    ignore (Utils.time_fn part2 lines)
  with e ->
    print_endline (Printexc.to_string e);
    raise e
