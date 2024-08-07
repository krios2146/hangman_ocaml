open Hangman_logic

let () =
  let game = init_game ~word:"random" ~max_mistakes:6 in
  start_game game
