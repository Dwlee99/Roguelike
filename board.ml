
type breakable = bool

type monster_type = 
  | Swordsman

(** The things that can occupy coordinates on the board. *)
type tile = 
  | Player
  | Wall of breakable
  | Empty
  | Monster of monster_type
  | Stairs

type t = tile array array

let tile_board t = Array.map Array.copy t

let set_tile board (x, y) tile =
  let width = Array.length board in
  let height = Array.length board.(0) in 
  if x >= 0 && x < width && y >= 0 && y < height 
  then board.(x).(y) <- tile

let get_tile board (x, y) = 
  let width = Array.length board in
  let height = Array.length board.(0) in 
  if x >= 0 && x < width && y >= 0 && y < height 
  then board.(x).(y) else Wall false


(** [add_outer_walls board] is a board identical to [board] except with
    each of the tiles that form the outer loop of the board being a wall. *)
let add_outer_walls board = 
  let width = Array.length board in
  let height = Array.length board.(0) in
  for x = 0 to width - 1 do
    board.(x).(0) <- Wall false;
    board.(x).(height - 1) <- Wall false;
  done;
  for y = 1 to height - 2 do
    board.(0).(y) <- Wall false;
    board.(width - 1).(y) <- Wall false;
  done;
  board

(** [empty_board width height] is an empty board with dimensions 
    [width] and [height]. *)
let empty_board width height =
  Array.make_matrix width height Empty

(** [randomize_tiles board] returns a board with the same with and height as 
    [board] but with each tile having an equal chance of being [Empty] or 
    [Wall].

    NOTE: This function was inspired by the roguelike board generating algorithm
    described on a blog post in Trystan's Blog 
    (http://trystans.blogspot.com/2011/08/roguelike-tutorial-03-scrolling-through.html). *)
let randomize_tiles board = 
  Random.self_init ();
  let width = Array.length board in
  let height = Array.length board.(0) in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      board.(x).(y) <- if Random.float 1.0 < 0.52 then Empty else Wall true;
    done;
  done;
  board

(** [smooth board] is the board [board] after undergoing one iteration of a 
    wall-smoothing algorithm. 

    NOTE: This function was inspired by the cave generating algorithm
    described on a blog post in Trystan's Blog 
    (http://trystans.blogspot.com/2011/08/roguelike-tutorial-03-scrolling-through.html). *)
let smooth board = 
  let width = Array.length board in
  let height = Array.length board.(0) in
  let new_board = empty_board width height in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      let empties = ref 0 in
      let walls = ref 0 in
      for ox = -1 to 1 do
        for oy = -1 to 1 do
          if x + ox < 0 || x + ox >= width || y + oy < 0 
             || y + oy >= height
          then ()
          else if (board.(x + ox).(y + oy) = Empty)
          then empties := 1 + !empties
          else walls := 1 + !walls
        done
      done;
      new_board.(x).(y) <- if empties >= walls then Empty else Wall true;
    done
  done;
  new_board

let in_bound t (x, y) =   
  let width = Array.length t in
  let height = Array.length t.(0) in 
  x >= 0 && x < width && y >= 0 && y < height 

let gen_board width height =
  let empty_board = empty_board width height in
  let random_board = ref (randomize_tiles empty_board) in
  for i = 0 to 5 do
    random_board := smooth !random_board; 
  done;
  let board_with_walls = add_outer_walls !random_board in
  board_with_walls


(* Path finding algorithm 1. *)

(** Node used for searching for path. *)
type node1 =
  {
    x : int;
    y : int;
    dist : int;
    prev: node1 option;
  }

let direction_to board cpos fpos max_dist =
  let queue = Queue.create () in 
  let (x, y) = cpos in 
  let (goal_x, goal_y) = fpos in 
  Queue.add {x = x; y = y; dist = 0; prev = None} queue;

  let dir_to node1 node2 =
    match node2.x - node1.x, node2.y - node1.y with
    | 0, 1 -> Action.Up
    | 1, 0 -> Action.Right
    | -1, 0 -> Action.Left
    | 0, -1 -> Action.Down
    | _ -> failwith "Nodes are not adjacent."
  in

  let rec best_dir node = 
    match node.prev with 
    | None -> None
    | Some n -> if n.prev = None then (Some (dir_to n node)) else best_dir n 
  in

  let dir = ref None in
  let found = ref false in
  let visited_board = Array.map (fun a -> Array.map (fun b -> false) a) board in

  let add_node queue x y dist prev =
    Queue.add {x = x; y = y; dist = dist; prev = Some prev} queue
  in

  let viable_node board visited_board x y distance prev =
    let tile = get_tile board (x, y) in
    let already_visited = visited_board.(x).(y) in 
    not(already_visited) && not(tile = Wall true) && not(tile = Wall false) 
    && (distance < max_dist)
  in

  let add_neighbors board n queue visited_board = 
    let coords x1 y1 = 
      [(x1 + 1, y1); (x1 - 1, y1); (x1, y1 + 1); (x1, y1 - 1)] in
    let coordinates = coords n.x n.y in
    List.iter (fun coord -> 
        if (viable_node board visited_board (fst coord) 
              (snd coord) (n.dist + 1) n)
        then add_node queue (fst coord) (snd coord) (n.dist + 1) n else ();
      ) coordinates;
  in

  while (not (Queue.is_empty queue)) && !found = false do
    let cur_node = Queue.pop queue in 
    visited_board.(cur_node.x).(cur_node.y) <- true;
    if (cur_node.x = goal_x && cur_node.y = goal_y)
    then (dir := (best_dir cur_node); found := true)
    else add_neighbors board cur_node queue visited_board;
  done;
  !dir