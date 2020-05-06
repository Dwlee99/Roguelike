open Graphics
exception End


type color_palette = {
  back_c: Graphics.color;
  fore_c: Graphics.color;

  black : Graphics.color;
  gray : Graphics.color;
  light_gray : Graphics.color;
  white : Graphics.color;
  yellow : Graphics.color;
  green : Graphics.color;
  orange : Graphics.color;
  violet : Graphics.color;
  magenta : Graphics.color;
  red : Graphics.color;
  blue : Graphics.color;
  cyan : Graphics.color;
}

let pal : color_palette = {
  black = rgb 39 40 34;
  gray = rgb 62 61 50;
  light_gray = rgb 117 113 94;
  white = rgb 248 248 242;
  yellow = rgb 230 219 116;
  green = rgb 166 226 46;
  orange = rgb 253 151 31;
  violet = rgb 174 129 255;
  magenta = rgb 253 95 240;
  red = rgb 249 38 114;
  cyan = rgb 161 239 228;
  blue = rgb 102 217 239;

  back_c = rgb 39 40 34;
  fore_c = rgb 166 266 46;
}

let init_screen_width = 1280
let init_screen_height = 720

let init_floor = 0

let game_state = ref (State.init_level)

let get_borders player_coords map_size= 
  let llx = (fst player_coords) - 40 in
  let lly = (snd player_coords) - 18 in
  let max_x = (fst map_size) - 80 in
  let max_y = (snd map_size) - 36 in
  let final_x = match llx with
    | x when x < 0 -> 0
    | x when x > max_x -> max_x
    | x -> x
  in
  let final_y = match lly with
    | y when y < 0 -> 0
    | y when y > max_y -> max_y
    | y -> y
  in (final_x, final_y)


let draw_game panel game =
  let board = State.tile_board game in
  match get_borders (State.get_player_pos game) (State.get_board_size game) 
  with
  | (start_col, start_row) -> 
    ignore(panel |> Ascii_panel.clear_graph);
    for col = start_col to start_col + 79 do
      for row = start_row to start_row + 35 do
        let charAndCol = match board.(col).(row) with
          | Player -> ('@', pal.green)
          | Wall _ -> (Char.chr 141, pal.blue)
          | Monster m -> (Char.chr 116, pal.red)
          | Empty -> (Char.chr 183, pal.gray)
          | Stairs -> (Char.chr 35, pal.yellow)
        in 
        ignore(Ascii_panel.draw_char (col-start_col) (row-start_row) 
                 (snd charAndCol) (fst charAndCol) panel)
      done
    done;
    Messages.draw_ui (State.get_stats game) 
      (State.get_msgs game) pal.white pal.light_gray;
    synchronize ()

let init_game () = 
  game_state := State.write_help !game_state;
  let t = Ascii_panel.open_window (init_screen_width) (init_screen_height) 
      pal.back_c 
          |> Ascii_panel.clear_graph 
          |> Ascii_panel.draw_point 0 0 pal.white 
  in draw_game t !game_state; t

let update action = 
  State.do_turn (!game_state) action

let stop_game panel =
  ignore(Ascii_panel.clear_graph panel);
  print_string "Thanks for playing... \n"

let res_key c (panel_info : Ascii_panel.t) =
  c |> Action.parse |> update

let res_exn ex : unit = 
  failwith "Game ending..."

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
      | e -> res_exn e
    done
  with
  | End -> f_end panel_info

(** [play_game f] starts the game. *)
let play_game () =
  game_loop (init_game) (stop_game) (res_key) (res_exn)

(* Execute the game engine. *)
let () = play_game ()