open Hangman_logic

let read_lines_to_list filename =
  try
    let in_channel = In_channel.open_text filename in
    let rec read_lines acc =
      match In_channel.input_line in_channel with
      | Some line -> read_lines (line :: acc)
      | None -> acc
    in
    let lines = read_lines [] in
    In_channel.close in_channel;
    lines
  with Sys_error msg ->
    Printf.eprintf "Error: %s\n" msg;
    []

let random_element lst =
  if lst = [] then None
  else
    let len = List.length lst in
    let index = Random.int len in
    Some (List.nth lst index)

let select_random_word filename : string option =
  let lines = read_lines_to_list filename in
  random_element lines

let () =
  Random.self_init ();
  let filename = "data/dictionary.txt" in
  let random_word = select_random_word filename in
  match random_word with
  | Some word ->
      let game = init_game ~word ~max_mistakes:6 in
      start_game game
  | None -> print_endline "No word was choosen"
