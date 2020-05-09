type damage = int

type monster_type = 
  | Swordsman

type monster = {
  name : string;
  position : (int * int);
  m_type : Board.monster_type;
  health : int;
  max_health : int;
  damage : int;
  exp : int;
  action_queue : m_action list;
  roaming_target : (int * int) ref;
}
and
  m_action = 
  | Wait of (monster -> monster)
  | Move of (monster -> Board.t -> (int * int) -> (monster))
  | Attack of (monster -> Board.t -> (int * int) -> (monster * damage))

module type Monster_Type = sig

  val create_monster : int -> monster

  val edit_queue : monster -> Board.t -> (int * int) -> m_action list

end

module type Edit_Monster = sig

  val create_monster : int -> monster

  val do_turn : monster -> Board.t -> (int * int) -> (monster * damage)

end

module Make_Monster (M : Monster_Type) : Edit_Monster = struct

  let create_monster = M.create_monster

  let do_action action monster board p_pos = 
    match action with
    | Wait f -> (f monster, 0)
    | Move f -> (f monster board p_pos, 0)
    | Attack f -> f monster board p_pos

  let do_turn monster board p_pos =
    let m = {monster with action_queue = M.edit_queue monster board p_pos} in
    match m.action_queue with
    | [] -> (m, 0)
    | h::t -> do_action h {monster with action_queue = t} board p_pos

end

let get_type m = m.m_type

(** [get_random_tile x y r] gets a random tile within [r] units of [(x, y)] *)
let get_random_tile x y r =
  let x_rel = Random.int (2 * r) in 
  let y_rel = Random.int (2 * r) in 
  (x + x_rel - r, y + y_rel - r)

(** [get_new_roaming_target m b r] gets a new roaming target for [m]. *)
let rec get_new_roaming_target monster board r =
  let monster_x = fst monster.position in 
  let monster_y = snd monster.position in 
  let new_target = get_random_tile monster_x monster_y r in
  match Board.direction_to board monster.position new_target r with
  | Some _ -> new_target
  | None -> get_new_roaming_target monster board r

let rec get_roam_direction monster board r = 
  let direction_to_roam = 
    Board.direction_to board monster.position !(monster.roaming_target) r in 
  match direction_to_roam with
  | Some d -> d
  | None -> 
    monster.roaming_target := get_new_roaming_target monster board r; 
    get_roam_direction monster board r