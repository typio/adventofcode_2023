let read_lines filename = 
  let rec aux ic acc =
  match input_line ic with
    | line -> aux ic (line :: acc)
    | exception End_of_file -> close_in ic; List.rev acc 
  in
  aux (open_in filename) []

let listify_string s = s |> String.to_seq |> List.of_seq
let stringify_list l = l |> List.to_seq |> String.of_seq
let reverse_string s = stringify_list (List.rev (listify_string s))
let is_digit c = c >= '0' && c <= '9'
let stringify_char c = String.make 1 c

let number_words = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine";]
let reversed_number_words = ["eno"; "owt"; "eerht"; "ruof"; "evif"; "xis"; "neves"; "thgie"; "enin";]

(* Part 1 *)
(* let () =
  try
    let lines = read_lines "day01.txt" in
      print_int (
        List.fold_left (+) 0 (
          List.map (fun line -> (
            int_of_string (
              stringify_char (List.find (fun c -> is_digit c) (listify_string line)) ^
              stringify_char (List.find (fun c -> is_digit c) (List.rev (listify_string line)))
            )
          )) lines
        )
      )
  with
  | e -> print_endline (Printexc.to_string e); raise e *)

(* Part 2 *)
let first_number line number_words =   
  let line_length = String.length line in
  (* i is the search pointer *)
  match 
    let rec aux i =
      if i >= line_length then None
      else if is_digit line.[i] then Some(stringify_char line.[i])
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
  with 
  | Some res -> res
  | None -> "0"

let () =
  try
    let lines = read_lines "day01.txt" in
      print_int (
        List.fold_left (+) 0 (
          List.map (fun line -> (
            int_of_string (
              (first_number line number_words) ^
              (first_number (reverse_string line) reversed_number_words)
            )
          )) lines
        )
      )
  with
  | e -> print_endline (Printexc.to_string e); raise e