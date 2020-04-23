open Yojson.Basic
open Graphics

(** [play_game f] starts the game using file [f]. *)
let play_game f =
  open_graph " 1280x720";
  print_string ((Printf.sprintf "%c" (read_key ()))^"\n")

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