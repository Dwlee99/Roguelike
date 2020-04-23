open Yojson.Basic
open Graphics
exception End

type state = {

  back_c: Graphics.color;
  fore_c: Graphics.color;
  point_c: Graphics.color;
}

let st : state = {
  back_c = Graphics.black;
  fore_c = Graphics.green;
  point_c = Graphics.white;
}

let init_screen_width = 1280
let init_screen_height = 720

let init_game s = 
  let t = Ascii_panel.open_window (init_screen_width) (init_screen_height) in
  t |> 
  Ascii_panel.fill_rect 0 0 (init_screen_width + 1) (init_screen_height + 1) 
    s.back_c 
  |> 
  Ascii_panel.draw_point 0 0 s.point_c

let stop_game s =
  close_graph ();
  print_string "Thanks for playing... \n"

let res_key c s =
  failwith "unimplemented"

let res_exn ex s = 
  failwith "unimplemented"

let game_loop f_init f_end f_key f_exn = 
  let panel_info = f_init st in
  try
    while true do
      try
        let s = wait_next_event [Key_pressed] in
        if s.Graphics.keypressed 
        then f_key s.Graphics.key st
      with
      | End -> raise End
      | e -> f_exn e st
    done
  with
  | End -> f_end st



(** [play_game f] starts the game. *)
let play_game () =
  game_loop (init_game) (stop_game) (res_key) (res_exn)

(* Execute the game engine. *)
let () = play_game ()