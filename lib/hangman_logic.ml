type game = {
  word : string;
  word_mask : string;
  used_letters : char list;
  max_mistakes : int;
  mistakes : int;
  guessed_letter : char option;
}

let init_game ~(word : string) ~(max_mistakes : int) : game =
  {
    word;
    word_mask = String.make (String.length word) '*';
    used_letters = [];
    max_mistakes;
    mistakes = 0;
    guessed_letter = None;
  }

let hangman_stages =
  [
    "\n  +---+\n  |   |\n      |\n      |\n      |\n      |\n=========\n";
    "\n  +---+\n  |   |\n  0   |\n      |\n      |\n      |\n=========";
    "\n  +---+\n  |   |\n  0   |\n  |   |\n      |\n      |\n=========";
    "\n  +---+\n  |   |\n  0   |\n /|   |\n      |\n      |\n=========";
    "\n  +---+\n  |   |\n  0   |\n /|\\  |\n      |\n      |\n=========";
    "\n  +---+\n  |   |\n  0   |\n /|\\  |\n /    |\n      |\n=========";
    "\n  +---+\n  |   |\n  0   |\n /|\\  |\n / \\  |\n      |\n=========";
  ]

let print_hangman (game : game) : unit =
  print_endline (List.nth hangman_stages game.mistakes);
  flush stdout

let print_state (game : game) : unit =
  Printf.printf
    "word = %s, mistakes = %d, max_mistakes = %d, used_letters = %s\n"
    game.word_mask game.mistakes game.max_mistakes
    (String.of_seq (List.to_seq game.used_letters));
  flush stdout

let rec get_user_input (game : game) : char =
  Printf.printf "Please enter a letter: ";
  flush stdout;

  let input = input_line stdin in

  if List.mem input.[0] game.used_letters then (
    print_endline "You already used this letter, try a new one";
    get_user_input game)
  else input.[0]

let contains_char str target_char = String.exists (fun c -> c = target_char) str

let get_new_word_mask (game : game) : string =
  String.mapi
    (fun i c ->
      match game.guessed_letter with
      | Some guessed_char -> if c = guessed_char then c else game.word_mask.[i]
      | None -> game.word_mask.[i])
    game.word

let guess_letter (game : game) (letter : char) : game =
  let updated_game =
    {
      game with
      guessed_letter = Some letter;
      used_letters = letter :: game.used_letters;
    }
  in
  if contains_char game.word letter then
    let new_word_mask = get_new_word_mask updated_game in
    { updated_game with word_mask = new_word_mask }
  else { updated_game with mistakes = game.mistakes + 1 }

let is_game_lost (game : game) : bool = game.mistakes >= game.max_mistakes
let is_game_won (game : game) : bool = not (contains_char game.word_mask '*')

let rec start_game (game : game) : unit =
  if not (is_game_lost game || is_game_won game) then
    let () = print_hangman game in
    let () = print_state game in
    let letter = get_user_input game in
    let updated_game = guess_letter game letter in
    start_game updated_game
  else if is_game_lost game then
    Printf.printf "Unfortunately you lost. The word was: %s\n" game.word
  else print_endline "You won, congrats"
