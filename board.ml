
type breakable = bool

type monster_type = 
  | Swordsman

(** The things that can occupy coordinates on the board. *)
type tile = 
  | Player
  | Wall of breakable
  | Empty
  | Monster of monster_type

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


let gen_board width height =
  let empty_board = empty_board width height in
  let random_board = ref (randomize_tiles empty_board) in
  for i = 0 to 5 do
    random_board := smooth !random_board; 
  done;
  let board_with_walls = add_outer_walls !random_board in
  board_with_walls

type distance = 
  | Int of int
  | Infinity

type node = {
  x : int;
  y : int;
  mutable prev : node option;
  mutable dist : distance;
  weight : distance;
}

let in_bound t (x, y) =   
  let width = Array.length t in
  let height = Array.length t.(0) in 
  x >= 0 && x < width && y >= 0 && y < height 

let get_node nodeArray (x, y) =
  let width = Array.length nodeArray in
  let height = Array.length nodeArray.(0) in 
  if x >= 0 && x < width && y >= 0 && y < height 
  then Some nodeArray.(x).(y) else None

let get_neighbors nA (x, y) =
  get_node nA (x + 1, y) :: get_node nA (x - 1, y) :: get_node nA (x, y + 1) :: 
  get_node nA (x, y - 1) :: []

let (++) dist1 dist2 =
  match dist1, dist2 with
  | Infinity, _ -> Infinity
  | _, Infinity -> Infinity
  | Int n1, Int n2 -> Int (n1 + n2)

let (+<) dist1 dist2 =
  match dist1, dist2 with
  | _, Infinity -> true
  | Infinity, _ -> false
  | Int n1, Int n2 -> n1 < n2

exception Exit_array

let path_to board c_pos t_pos =
  let queue = Queue.create () in
  let (sX, sY) = c_pos in
  let width = Array.length board in
  let height = Array.length board.(0) in 
  if sX < 0 || sX >= width || sY < 0 || sY >= height 
  then None else 
    let nodeArray = Array.mapi (fun x a -> Array.mapi (fun y b -> 
        let node = {x = x; y = y; prev = None; dist = Infinity; 
                    weight = if board.(x).(y) = Empty then Int 1 else Infinity}
        in Queue.add node queue; node
      ) a) board in
    let source = nodeArray.(sX).(sY) in
    source.dist <- Int 0;
    let current = ref source in
    try
      while not (Queue.is_empty queue) do
        let n = Queue.pop queue in
        current := n;
        let (cX, cY) = (!current.x, !current.y) in
        if (cX, cY) = t_pos then raise Exit_array else
          let neighbors = get_neighbors nodeArray (cX, cY) in
          List.iter (fun node -> 
              match node with
              | None -> ()
              | Some n -> begin
                  let new_dist = !current.dist ++ !current.weight in
                  if new_dist +< n.dist then (n.dist <- new_dist; n.prev <- Some !current) 
                end
            ) neighbors
      done;
      None
    with Exit_array -> 
      let list = ref [] in 
      while (!current) <> source do
        let prev = match !current.prev with
          | None -> failwith "This can't happen"
          | Some n -> n
        in
        current := prev;
        let (x, y) = (prev.x, prev.y) in
        list := (x, y) :: !list;
      done;
      Some !list