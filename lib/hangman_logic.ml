type game = {
  word : string;
  word_mask : string;
  used_letters : char list;
  max_mistakes : int;
  mistakes : int;
  guessed_letter : char;
}

let init_game ~(word : string) ~(max_mistakes : int) : game =
  {
    word;
    word_mask = String.make (String.length word) '*';
    used_letters = [];
    max_mistakes;
    mistakes = 0;
    guessed_letter = ' ';
  }

let zero_mistake_hangman = "qwe"
let one_mistake_hangman = "qwe"
let two_mistake_hangman = "qwe"
let three_mistake_hangman = "qwe"
let four_mistake_hangman = "qwe"
let five_mistake_hangman = "qwe"
let six_mistake_hangman = "qwe"

let print_hangman (game : game) : game =
  (match game.mistakes with
  | 0 -> print_string zero_mistake_hangman
  | 1 -> print_string one_mistake_hangman
  | 2 -> print_string two_mistake_hangman
  | 3 -> print_string three_mistake_hangman
  | 4 -> print_string four_mistake_hangman
  | 5 -> print_string five_mistake_hangman
  | 6 -> print_string six_mistake_hangman
  | _ -> print_string "Error while displaying hangman");
  game

let print_state (game : game) : game =
  Printf.printf
    "word = %s, mistakes = %s, max_mistakes = %s, used_letters = %s\n"
    game.word_mask
    (string_of_int game.mistakes)
    (string_of_int game.max_mistakes)
    (String.of_seq (List.to_seq game.used_letters));
  game

let rec get_user_input (game : game) : game =
  Printf.printf "Please enter a letter: ";

  let input = input_char stdin in

  if List.mem input game.used_letters then (
    print_endline "You already used this letter, try a new one";
    get_user_input game)
  else
    {
      game with
      guessed_letter = input;
      used_letters = input :: game.used_letters;
    }

let contains_char str target_char = String.exists (fun c -> c = target_char) str

let get_new_word_mask (game : game) : string =
  let new_mask =
    String.mapi
      (fun i c -> if c = game.guessed_letter then c else game.word_mask.[i])
      game.word
  in
  new_mask

let guess_letter (game : game) : game =
  if contains_char game.word game.guessed_letter then
    let new_word_mask = get_new_word_mask game in
    { game with word_mask = new_word_mask }
  else { game with mistakes = game.mistakes + 1 }

let is_game_lost (game : game) : bool =
  if game.mistakes >= game.max_mistakes then true else false

let is_game_won (game : game) : bool =
  if contains_char game.word_mask '*' then false else true

let is_game_continues (game : game) : bool =
  not (is_game_lost game || is_game_won game)

let rec start_game (game : game) : unit =
  if is_game_continues game then
    game |> print_hangman |> print_state |> get_user_input |> guess_letter
    |> start_game
  else if is_game_lost game then
    Printf.printf "Unfortunately you're lost. The word was: %s\n" game.word
  else print_endline "You won, congrats"
