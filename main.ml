open Yojson.Basic
open Graphics

(** [play_game f] starts the game using file [f]. *)
let play_game f =
  print_string "Welcome to our roguelike! it's still being implemented.\n";
  open_graph " 300x200"

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to [Roguelike name]\n");
  print_endline "Press [Enter] to play!";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | x -> play_game x

(* Execute the game engine. *)
let () = main ()