open Graphics
open State

let msg_list = []

let msg_x = 6

let msg_y = 700

let write_msg msg col =
  Graphics.set_color col;
  Graphics.moveto msg_x msg_y;
  Graphics.draw_string msg

let update_stats state stat_col = 
  failwith "unimplemented"

let draw_divider col = 
  Graphics.set_color col;
  Graphics.fill_rect 0 641 3 79;
  Graphics.fill_rect 0 641 1280 3;
  Graphics.fill_rect 1277 641 3 79;
  Graphics.fill_rect 0 716 1280 3;
  Graphics.fill_rect 500 641 3 79

let draw_ui state msg_col stat_col div_col = 
  draw_divider div_col;
  update_stats state stat_col