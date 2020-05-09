open Graphics
exception End
exception PlayAgain


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

let game_state = ref (State.init_level () )

(** [get_borders pc m_size] is [(start_col, start_row)] where these values are
    the first column and row to be drawn on the ascii_panel. *)
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

(** A function that will draw at the end of each call to [draw_game] and then
    be returned to its default function. *)
let delayed_draw = ref (fun (x, y) -> ())

(** [draw_tile col row tile panel] draws a single tile at [(col, row)] on
    [panel]. *)
let draw_tile col row tile panel =
  let (chr, color) = match tile with
    | Board.Player -> ('@', pal.green)
    | Wall _ -> (Char.chr 141, pal.blue)
    | Monster Board.Swordsman -> ('m', pal.red)
    | Monster Board.Ranger -> (Char.chr 172, pal.orange)
    | Empty -> (Char.chr 183, pal.gray)
    | Stairs -> (Char.chr 35, pal.yellow)
    | Weapon Board.BattleAxe -> ('Y', pal.magenta)
    | Weapon Board.ShortBow -> ('D', pal.magenta)
    | Weapon Board.ShortSword -> ('t', pal.magenta)
    | Armor _ -> ('H', pal.light_gray)
  in 
  ignore(Ascii_panel.draw_char (col) (row) color chr panel)

(** [draw_game panel game] draws the current state of [game] on [panel]. *)
let draw_game panel game =
  let board = State.tile_board game in
  match get_borders (State.get_player_pos game) (State.get_board_size game) 
  with
  | (start_col, start_row) -> 
    ignore(panel |> Ascii_panel.clear_graph);
    for col = start_col to start_col + 79 do
      for row = start_row to start_row + 35 do
        draw_tile (col - start_col) (row - start_row) board.(col).(row) panel
      done
    done;
    !delayed_draw (start_col, start_row);
    delayed_draw := (fun h -> ());
    Messages.draw_ui (State.get_stats game) 
      (State.get_msgs game) pal.violet pal.violet pal.light_gray;
    synchronize ()

let init_game () = 
  game_state := State.write_help !game_state;
  let t = Ascii_panel.open_window (init_screen_width) (init_screen_height) 
      pal.back_c 
          |> Ascii_panel.clear_graph 
          |> Ascii_panel.draw_point 0 0 pal.white 
  in draw_game t !game_state; t

(** [update pan action] updates [game_state] with the execution of [action]. *)
let update pan action = 
  match action with
  | Action.Modify m -> State.do_turn !game_state m
  | Action.Display d -> 
    delayed_draw := (fun h -> State.do_display !game_state d pan h); 
    State.display_update !game_state d
  | Quit -> raise End
  | None -> !game_state

let stop_game panel =
  ignore(Ascii_panel.clear_graph panel);
  print_string "Thanks for playing... \n"

(** [get_key] waits for a key press and returns the character once one is
    pressed. *)
let get_key () = (wait_next_event [Key_pressed]).Graphics.key

let res_key c (panel_info : Ascii_panel.t) =
  let first_action = c |> Action.parse in
  match first_action with
  | Display Help -> State.write_help !game_state
  | Display PlayerHelp -> State.write_player_help !game_state
  | Display FightingHelp -> State.write_fighting_help !game_state
  | Display Inv -> State.write_inventory !game_state
  | Display Melee -> ignore ((update panel_info) (Display Melee));
    draw_game panel_info !game_state; 
    get_key () |> Action.parse_two c |> (update panel_info)
  | Display Ranged -> ignore ((update panel_info) (Display Ranged));
    draw_game panel_info !game_state; 
    get_key () |> Action.parse_two c |> (update panel_info)
  | Quit -> raise End
  | _ -> first_action |> (update panel_info)

let res_exn ex : unit = 
  failwith "Game ending..."

(** [resize_to_proper ()] resizes the window to its initial screen size. *)
let resize_to_proper () = 
  if not (size_x () = init_screen_width) || 
     not (size_y () = init_screen_height) 
  then resize_window init_screen_width init_screen_height 
  else () 

(** [run_turn res_key panel_info] gets the next keypressed and resolves it to
    an action using [res_key]. It then draws the game with the new info onto
    [panel_info]. *)
let run_turn res_key panel_info =
  let c = get_key () in
  resize_to_proper ();
  game_state := res_key c panel_info;
  draw_game panel_info !game_state

let rec game_loop f_init f_end f_key f_exn = 
  let panel_info = f_init () in
  try
    while true do
      try
        run_turn f_key panel_info;
      with
      | End -> raise End
      | State.PlayerDeath -> raise State.PlayerDeath
      | e -> f_exn e
    done
  with
  | End -> f_end panel_info
  | State.PlayerDeath -> initiate_death panel_info

(** [initiate_death panel_info] begins the death screen. *)
and initiate_death panel_info =
  resize_to_proper ();
  game_state := State.write_msgs !game_state ["You died."]; 
  draw_game panel_info !game_state;
  post_death_screen !game_state panel_info

(** [post_death_screen game] shows the stats of the player after they die and
    offers them the opportunity to play again. *)
and post_death_screen game panel = 
  ignore(Ascii_panel.clear_graph panel);
  let player_stats = State.get_stats game in
  Graphics.set_color pal.white;
  Graphics.moveto 610 420;
  Graphics.draw_string "You died!";
  Graphics.moveto 585 400;
  Graphics.draw_string "Here's how you did:";
  Graphics.moveto 585 380;
  Graphics.draw_string
    ("Turns Lived: " ^ (string_of_int player_stats.turns_played));
  Graphics.moveto 555 300;
  Graphics.draw_string "Press any key to play again!";
  synchronize ();
  try
    let _ = get_key () in
    game_state := State.init_level (); play_game ()
  with 
  | _ -> 
    ignore(Ascii_panel.clear_graph panel);
    print_string "Thanks for playing... \n"
(** [play_game f] starts the game. *)
and play_game () =
  game_loop (init_game) (stop_game) (res_key) (res_exn)

(* Execute the game engine. *)
let () = play_game ()