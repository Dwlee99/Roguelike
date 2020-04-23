open Yojson.Basic
open Graphics

exception End

type state = {
  max_x: int;
  max_y: int;
  x: int ref;
  y: int ref;
  x_scale: int;
  y_scale: int;
  back_c: Graphics.color;
  fore_c: Graphics.color;
  point_c: Graphics.color;
}

let init_state : state = {
  max_x = 142;
  max_y = 60;
  x = ref 0;
  y = ref 0;
  x_scale = 9;
  y_scale = 12;
  back_c = Graphics.black;
  fore_c = Graphics.green;
  point_c = Graphics.white;
}

let draw_point x y xs ys c =
  Graphics.set_color c;
  Graphics.fill_rect (xs * x) (ys * y) xs ys

let init_game s = 
  open_graph (" "^(string_of_int (s.x_scale * s.max_x))
              ^"x"^(string_of_int(s.y_scale * s.max_y)));
  set_color s.back_c;
  fill_rect 0 0 (s.x_scale * s.max_x + 1) (s.y_scale * s.max_y + 1);
  draw_point !(s.x) !(s.y) s.x_scale s.y_scale s.point_c

let stop_game s =
  close_graph ();
  print_string "Thanks for playing... \n"

let res_key c s =
  failwith "unimplemented"

let res_mouse x y s =
  failwith "unimplemented"

let res_exn ex s = 
  failwith "unimplemented"

let game_loop f_init f_end f_key f_mouse f_exn = 
  f_init init_state;
  try
    while true do
      try
        let s = wait_next_event [Button_down; Key_pressed] in
        if s.Graphics.keypressed 
        then f_key s.Graphics.key init_state
        else if s.Graphics.button 
        then f_mouse s.Graphics.mouse_x s.Graphics.mouse_y init_state
      with
      |End -> raise End
      | e -> f_exn e init_state
    done
  with
  |End -> f_end init_state



(** [play_game f] starts the game. *)
let play_game () =
  game_loop (init_game) (stop_game) (res_key) (res_mouse) (res_exn)

(* Execute the game engine. *)
let () = play_game ()