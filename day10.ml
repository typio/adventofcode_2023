#mod_use "utils.ml"

type position = { x : int; y : int }
type pipe = { pos : position; symbol : char }
type direction = Up | Right | Down | Left

(* which way you have to be going to get in; which way you'll be going coming out *)
type direction_travel = { in_dir : direction array; out_dir : direction array }

let pipe_direction = function
  | '-' -> { in_dir = [| Right; Left |]; out_dir = [| Right; Left |] }
  | '|' -> { in_dir = [| Up; Down |]; out_dir = [| Up; Down |] }
  | 'F' -> { in_dir = [| Up; Left |]; out_dir = [| Right; Down |] }
  | 'J' -> { in_dir = [| Down; Right |]; out_dir = [| Up; Left |] }
  | '7' -> { in_dir = [| Up; Right |]; out_dir = [| Left; Down |] }
  | 'L' -> { in_dir = [| Down; Left |]; out_dir = [| Right; Up |] }
  | _ -> { in_dir = [||]; out_dir = [||] }

let get_translation = function
  | Up -> (0, -1)
  | Right -> (1, 0)
  | Down -> (0, 1)
  | Left -> (-1, 0)

let make_maze lines = lines |> Array.map (fun line -> Utils.explode line)

let find_start maze =
  let _, start_position =
    Array.fold_left
      (fun (y, pos) row ->
        ( y + 1,
          Array.fold_left
            (fun pos (x, c) -> if c = 'S' then { x; y } else pos)
            pos
            (Array.mapi (fun i c -> (i, c)) row) ))
      (0, { x = -1; y = -1 })
      maze
  in
  start_position

let print_maze maze =
  Array.map
    (fun char_arr -> String.init (Array.length char_arr) (Array.get char_arr))
    maze
  |> Array.iter (Printf.printf "%s\n");
  flush stdout

let rec bfs maze i fronts =
  let new_fronts =
    let get_new_front x y directions =
      directions
      |> Array.map (fun dir ->
             (dir, get_translation dir) |> fun (dir, (dx, dy)) ->
             let x2 = x + dx in
             let y2 = y + dy in
             if
               x2 >= 0 && y2 >= 0
               && x2 < Array.length maze.(0)
               && y2 < Array.length maze
             then
               let new_symbol = maze.(y2).(x2) in
               if Array.exists (( = ) dir) (pipe_direction new_symbol).in_dir
               then Some { pos = { x = x2; y = y2 }; symbol = new_symbol }
               else None
             else None)
    in

    Array.fold_left
      (fun (acc : pipe array) front ->
        let x = front.pos.x in
        let y = front.pos.y in
        let current_pipe = maze.(y).(x) in
        maze.(y).(x) <- '$';
        (if current_pipe = 'S' then
           [| Up; Right; Down; Left |] |> get_new_front x y
         else (pipe_direction current_pipe).out_dir |> get_new_front x y)
        |> Utils.array_filter_some |> Array.append acc)
      [||] fronts
  in
  if Array.length new_fronts > 0 then bfs maze (i + 1) new_fronts else i

let part1 lines =
  let maze = make_maze lines in
  let start_position = find_start maze in

  bfs maze 0 [| { pos = start_position; symbol = 'S' } |] |> print_int

let part2 lines =
  (* Check commit history for the flood fill I accidentally did *)
  let maze = make_maze lines in
  let pipe_mask_maze = make_maze lines in
  let start_position = find_start pipe_mask_maze in

  let _ = bfs pipe_mask_maze 0 [| { pos = start_position; symbol = 'S' } |] in

  pipe_mask_maze
  |> Array.iteri (fun y row ->
         row |> Array.iteri (fun x c -> if c <> '$' then maze.(y).(x) <- '.'));

  match
    Array.fold_left
      (fun (y, sum) row ->
        ( y + 1,
          sum
          +
          match
            Array.fold_left
              (fun (x, i, s) c ->
                if c = '|' then (x + 1, i +. 1., s)
                  (* I just found my S is an F from looking at the input idk *)
                else if c = 'F' || c = 'J' || c = 'S' then (x + 1, i -. 0.5, s)
                else if c = 'L' || c = '7' then (x + 1, i +. 0.5, s)
                else if Float.rem i 2. <> 0. && c = '.' then (
                  maze.(y).(x) <- 'I';
                  (x + 1, i, s + 1))
                else (
                  maze.(y).(x) <- 'O';
                  (x + 1, i, s)))
              (0, 0., 0) row
          with
          | _, _, s -> s ))
      (0, 0) maze
  with
  | _, s -> s |> print_int

let () =
  try
    let lines = Utils.get_lines_arr "day10.txt" in
    Printf.printf "Timing Part 1\n";
    ignore (Utils.time_fn part1 lines);
    Printf.printf "Timing Part 2\n";
    ignore (Utils.time_fn part2 lines)
  with e ->
    print_endline (Printexc.to_string e);
    raise e
