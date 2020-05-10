open Graphics

type game_unit = {
  char_width : int;
  char_height : int;
}

type t = {
  screen_width : int;
  screen_height : int;
  background_color : Graphics.color;
  sizes : game_unit;
}

let board_font = "consolas"

let open_window w h bcolor = 
  open_graph (" " ^ (string_of_int w) ^ "x" ^ (string_of_int h));
  set_window_title "CS 3110 Project : Roguelike";
  auto_synchronize false;
  {
    screen_width = w;
    screen_height = h;
    background_color = bcolor;
    sizes = {
      char_width = w / 80;
      char_height = h / 40;
    }
  }

let draw_point x y c t =
  set_color c;
  fill_rect (t.sizes.char_width * x) (t.sizes.char_height * y) 
    t.sizes.char_width t.sizes.char_height;
  t

let draw_char x y c chr t = 
  set_color c;
  (*set_font board_font;*)
  moveto (t.sizes.char_width * x) (t.sizes.char_height * y);
  draw_char chr;
  t

let fill_rect x y w h c t =
  set_color c;
  fill_rect (t.sizes.char_width * x) (t.sizes.char_height * y) 
    (t.sizes.char_width * w) (t.sizes.char_width * h);
  t

(** The ratios to which a rectangle's bottom left corner need to be shifted
    to the left. *)
let rect_x_offset = 0.35
(** The ratios to which a rectangle's bottom left corner need to be shifted
    down. *)
let rect_y_offset = 0.1 

let rec outline_coords list c t =
  set_color c;
  match list with
  | [] -> t
  | (x, y) :: tail ->   
    let (f1, f2) = 
      (float_of_int t.sizes.char_width *. (float_of_int x -. rect_x_offset),
       float_of_int t.sizes.char_height *. (float_of_int y -. rect_y_offset))
    in
    let (x1, y1) = (int_of_float f1, int_of_float f2) in
    draw_rect x1 y1 t.sizes.char_width t.sizes.char_height; 
    outline_coords tail c t

let clear_graph t =
  fill_rect 0 0 t.screen_width t.screen_height t.background_color t