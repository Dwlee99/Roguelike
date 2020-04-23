open Yojson.Basic
open Graphics

exception End

type state = {
  max_x: int;
  max_y: int;
  mutable x: int;
  mutable y: int;
  x_scale: int;
  y_scale: int;
  back_c: color;
  fore_c: color;
  point_c: color;
}

let init = 
  open_graph " 1280x720"

let terminate s =
  close_graph ();
  print_string "Thanks for playing... \n"

let res_key c s =
  failwith "unimplemented"

let res_mouse x y s =
  failwith "unimplemented"

let res_exn ex s = 
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
let play_game =
  game_loop init terminate res_key res_mouse res_exn

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  play_game

(* Execute the game engine. *)
let () = main ()