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

let st : state = {
  max_x = 71;
  max_y = 30;
  x = ref 0;
  y = ref 0;
  x_scale = 18;
  y_scale = 24;
  back_c = Graphics.black;
  fore_c = Graphics.green;
  point_c = Graphics.white;
}

let draw_point x y xs ys c =
  set_color c;
  fill_rect (xs * x) (ys * y) xs ys

let draw_char x y xs ys col chr = 
  failwith "unimplemented"

let clear_area x1 y1 x2 y2 c = 
  failwith "unimplemented"

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

let res_exn ex s = 
  failwith "unimplemented"

let game_loop f_init f_end f_key f_exn = 
  f_init st;
  try
    while true do
      try
        let s = wait_next_event [Key_pressed] in
        if s.Graphics.keypressed 
        then f_key s.Graphics.key st
      with
      |End -> raise End
      | e -> f_exn e st
    done
  with
  |End -> f_end st



(** [play_game f] starts the game. *)
let play_game () =
  game_loop (init_game) (stop_game) (res_key) (res_exn)

(* Execute the game engine. *)
let () = play_game ()