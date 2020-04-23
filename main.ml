open Yojson.Basic
open Graphics

exception End

let init = 
  failwith "unimplemented"

let terminate =
  failwith "unimplemented"

let res_key c =
  failwith "unimplemented"

let res_mouse x y =
  failwith "unimplemented"

let res_exn ex = 
  failwith "unimplemented"

let game_loop f_init f_end f_key f_mouse f_exn = 
  f_init ();
  try
    while true do
      try
        let s = wait_next_event [Button_down; Key_pressed] in
        if s.Graphics.keypressed 
        then f_key s.Graphics.key
        else if s.Graphics.button 
        then f_mouse s.Graphics.mouse_x s.Graphics.mouse_y
      with
      |End -> raise End
      | e -> f_exn e
    done
  with
  |End -> f_end



(** [play_game f] starts the game using file [f]. *)
let play_game f =
  open_graph " 1280x720";
  game_loop init terminate res_key res_mouse res_exn

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  play_game

(* Execute the game engine. *)
let () = main ()