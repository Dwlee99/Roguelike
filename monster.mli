(** Represents generic information for each Monster that will be in the game.*)

(** Represents the way damage is computed. *)
type damage = int

(** A representation of monsters in the game and the info they need.
    An [m_action] is a representation of an action that can be taken by a 
    monster. *)
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
and m_action = 
  | Wait of (monster -> monster)
  | Move of (monster -> Board.t -> (int * int) -> (monster))
  | Attack of (monster -> Board.t -> (int * int) -> (monster * damage))

(** Modules of this type define a new monster to be added to the game. *)
module type Monster_Type = sig
  (** [create_monster l] creates a monster with level [l]. *)
  val create_monster : int -> monster

  (** [edit_queue m b (pX, pY)] returns a list of actions to be taken by the
      monster. The head of the list is the first action that will be taken.*)
  val edit_queue : monster -> Board.t -> (int * int) -> m_action list

end

(** Modules of this type are complete monsters that can be created and have
    turns. *)
module type Edit_Monster = sig
  (** [create_monster l] creates a monster with level [l]. *)
  val create_monster : int -> monster

  (** [do_turn m b (pX, pY)] performs the monster's head action and returns
      the new monster representation and the amount of damage done to the 
      player. *)
  val do_turn : monster -> Board.t -> (int * int) -> (monster * damage)

end

(** [Make_Monster M] takes in a [Monster_Type] and creates a [Edit_Monster]
    using the front of [action_queue] to define what the monster's next turn
    will do.*)
module Make_Monster (M : Monster_Type) : Edit_Monster

(** [get_type m] is [m.m_type]. *)
val get_type : monster -> Board.monster_type

(** [get_roam_direction m b r] uses a [m]'s existing [roam_target] or
    computes its own within a radius [r] of [m] and returns the direction [m]
    must move to get to the target fastest. *)  
val get_roam_direction : monster -> Board.t -> int -> Action.direction