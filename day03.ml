(* Find index list for symbols in a line *)
let find_symbols line_char_list =
  List.mapi
    (fun i e -> if Utils.is_digit e || e = '.' then -1 else i)
    line_char_list
  |> List.filter (fun e -> e <> -1)

(* Find (number: string, line_index, first_index, last_index) list for every number in a line *)
let find_numbers (line_index : int) line_char_list =
  let rec aux i number_start_i number_string pairs = function
    | c :: rest ->
        if Utils.is_digit c then
          let new_number_string = number_string ^ Char.escaped c in
          aux (i + 1)
            (Some (match number_start_i with Some s_i -> s_i | None -> i))
            new_number_string pairs rest
        else
          let new_pairs =
            match number_start_i with
            | Some s_i -> (number_string, line_index, s_i, i - 1) :: pairs
            | None -> pairs
          in
          aux (i + 1) None "" new_pairs rest
    | [] -> (
        match number_start_i with
        | Some s_i -> (number_string, line_index, s_i, i - 1) :: pairs
        | None -> pairs)
  in
  List.rev (aux 0 None "" [] line_char_list)

let safe_nth lst n default_value =
  if n < 0 then default_value
  else Option.value ~default:default_value (List.nth_opt lst n)

let part1 lines =
  (* Compile a list of every number and its location per line *)
  let number_locations =
    List.flatten
      (List.mapi
         (fun i line -> find_numbers i (Utils.listify_string line))
         lines)
  in

  (* For every line of numbers check the surrounding three lines for touching symbols *)
  List.fold_left ( + ) 0
    (List.map
       (fun num ->
         let num_value = match num with a, b, c, d -> int_of_string a in
         let num_line = match num with a, b, c, d -> b in
         let num_start_i = match num with a, b, c, d -> c in
         let num_end_i = match num with a, b, c, d -> d in

         if
           List.exists (* If there exists neighboring line fulfilling predicate *)
               (fun line ->
               List.exists (* If one of the symbols in line touches number *)
                 (fun symbol_i ->
                   symbol_i >= num_start_i - 1 && symbol_i <= num_end_i + 1)
                 (find_symbols (Utils.listify_string line)))
             [
               safe_nth lines (num_line - 1) "";
               safe_nth lines num_line "";
               safe_nth lines (num_line + 1) "";
             ]
         then num_value
         else 0)
       number_locations)
  |> print_int

let find_gears (line_index : int) line_char_list =
  List.mapi
    (fun i e -> if e = '*' then (i, line_index) else (-1, line_index))
    line_char_list
  |> List.filter (fun e -> match e with v, li -> v <> -1)

let part2 lines =
  let gear_locations =
    List.flatten
      (List.mapi (fun i line -> find_gears i (Utils.listify_string line)) lines)
  in

  (* For every line of numbers check the surrounding three lines for touching symbols *)
  List.fold_left ( + ) 0
    (let nums =
       List.filter_map
         (fun gear ->
           let gear_i, line_i = gear in
           let factors =
             List.flatten
               ((* For every gear *)
                (List.map (fun line ->
                     (* For every line surrounding gear *)
                     List.filter_map
                       (fun num ->
                         (* For every number in a line*)
                         let num_value =
                           match num with a, b, c, d -> int_of_string a
                         in
                         let num_start_i = match num with a, b, c, d -> c in
                         let num_end_i = match num with a, b, c, d -> d in
                         (* If it touches the gear: store the number*)
                         if gear_i >= num_start_i - 1 && gear_i <= num_end_i + 1
                         then Some num_value
                         else None)
                       (find_numbers (-1) (Utils.listify_string line))))
                  [
                    safe_nth lines (line_i - 1) "";
                    safe_nth lines line_i "";
                    safe_nth lines (line_i + 1) "";
                  ])
           in
           if List.length factors >= 2 then
             Some (List.fold_left ( * ) 1 factors)
           else None)
         gear_locations
     in
     nums)
  |> print_int

let () =
  try
    let lines = Utils.read_lines "day03.txt" in
    Printf.printf "Timing Part 1\n";
    ignore (Utils.time_fn part1 lines);
    Printf.printf "Timing Part 2\n";
    ignore (Utils.time_fn part2 lines);
  with e ->
    print_endline (Printexc.to_string e);
    raise e