open Yojson.Basic
open Graphics
exception End


type color_palette = {

  back_c: Graphics.color;
  fore_c: Graphics.color;
  point_c: Graphics.color;
}

let pal : color_palette = {
  back_c = Graphics.black;
  fore_c = Graphics.green;
  point_c = Graphics.white;
}

let init_screen_width = 1280
let init_screen_height = 720

let init_column_count = 80
let init_row_count = 40

let game_state = ref (State.init_game init_column_count init_row_count)

let draw_game panel game =
  let board = State.tile_board game in
  ignore(panel |> Ascii_panel.clear_graph);
  for col = 0 to init_column_count - 1 do
    for row = 0 to init_row_count - 1 do
      let charToDraw = match board.(col).(row) with
        | Player -> '@'
        | Wall -> '#'
        | Empty -> ' '
      in 
      ignore(Ascii_panel.draw_char col row pal.fore_c charToDraw panel)
    done
  done;
  synchronize ()

let init_game () = 
  let t = Ascii_panel.open_window (init_screen_width) (init_screen_height) 
      pal.back_c 
          |> Ascii_panel.clear_graph 
          |> Ascii_panel.draw_point 0 0 pal.point_c 
  in draw_game t !game_state; t

let update action = 
  State.update (!game_state) action

let stop_game panel =
  ignore(Ascii_panel.clear_graph panel);
  print_string "Thanks for playing... \n"

let res_key c (panel_info : Ascii_panel.t) =
  c |> Action.parse |> update

let res_exn ex : unit = 
  failwith "unimplemented"

let game_loop f_init f_end f_key f_exn = 
  let panel_info = f_init () in
  try
    while true do
      try
        let s = wait_next_event [Key_pressed] in
        let _ = if not (size_x () = init_screen_width) || 
                   not (size_y () = init_screen_height) 
          then resize_window init_screen_width init_screen_height 
          else () in
        game_state := res_key s.Graphics.key panel_info;
        draw_game panel_info !game_state;
        (*if s.Graphics.keypressed 
          then ignore(f_key s.Graphics.key panel_info)*)
      with
      | End -> raise End
      | e -> f_exn e
    done
  with
  | End -> f_end panel_info

(** [play_game f] starts the game. *)
let play_game () =
  game_loop (init_game) (stop_game) (res_key) (res_exn)

(* Execute the game engine. *)
let () = play_game ()