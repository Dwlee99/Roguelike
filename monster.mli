type damage = int

type monster_type = 
  | Swordsman

type monster = {
  name : string;
  position : State.coordinate option;
  health : int;
  max_health : int;
  damage : int;
  action_queue : m_action list;
}
and
  m_action = 
  | Wait of (monster -> monster)
  | Move of (monster -> Board.t -> State.coordinate -> (monster))
  | Attack of (monster -> Board.t -> State.coordinate -> (monster * damage))

module type Monster_Type = sig

  val create_monster : int -> monster

  val edit_queue : monster -> Board.t -> State.coordinate -> m_action list

end

module type Edit_Monster = sig

  val create_monster : int -> monster

  val do_turn : monster -> Board.t -> State.coordinate -> (monster * damage)

end

module Make_Monster (M : Monster_Type) : Edit_Monster

(** [path_to_player board m_pos n_pos] is the shortest path to n_pos from m_pos.
*)
val path_to : Board.t -> State.coordinate -> State.coordinate -> State.coordinate list