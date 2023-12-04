let part1 lines = 
  print_int (
    List.fold_left (+) 0 (
      List.map (fun line -> (
        int_of_string (
          Utils.stringify_char (List.find (fun c -> Utils.is_digit c) (Utils.listify_string line)) ^
          Utils.stringify_char (List.find (fun c -> Utils.is_digit c) (List.rev (Utils.listify_string line)))
        )
      )) lines
    )
  )

let number_words = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine";]
let reversed_number_words = ["eno"; "owt"; "eerht"; "ruof"; "evif"; "xis"; "neves"; "thgie"; "enin";]

let first_number line number_words =   
  let line_length = String.length line in
  (* i is the search pointer *)
    let rec aux i =
      if i >= line_length then None
      else if Utils.is_digit line.[i] then Some(Utils.stringify_char line.[i])
      else match find_number_word i 0 number_words with
      | Some word -> Some (string_of_int word)
      | None -> aux (i + 1)

    (* return index of matching word +1 *)
    and find_number_word i word_index = function
      | [] -> None
      | word :: ws -> 
        let word_length = String.length word in  
        if  i + word_length <= line_length && String.sub line i word_length = word then Some(word_index + 1)
        else find_number_word i (word_index + 1) ws
    in aux 0 

let part2 lines = 
  print_int (
    List.fold_left (+) 0 (
      List.map (fun line -> (
        int_of_string (
          Option.get (first_number line number_words) ^
          Option.get (first_number (Utils.reverse_string line) reversed_number_words)
        )
      )) lines
    )
  )

let () =
  try
    let lines = Utils.read_lines "day01.txt" in
    Printf.printf "Timing Part 1\n";
    ignore (Utils.time_fn part1 lines);
    Printf.printf "Timing Part 2\n";
    ignore (Utils.time_fn part2 lines)
  with
  | e -> print_endline (Printexc.to_string e); raise e
