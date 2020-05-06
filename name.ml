open Yojson.Basic.Util
open Random

(** The json file from which we will pull our list of names. *)
let json_file = "names.json"

(** [parse_list f l] is the list containing each element of [l] parsed with 
    the function [f]. *)
let rec parse_list f (l : Yojson.Basic.t list) = 
  match l with 
  | [] -> []
  | h::t -> (f h)::(parse_list f t)

(** [get_names_list ()] gets the list of names from the json file specified
    above. *)
let get_names_list () = 
  let json = Yojson.Basic.from_file json_file in 
  json |> member "names" |> to_list |> parse_list to_string


(** [random_name ()] is a random name for a monster. *)
let random_name () = 
  let names = get_names_list () in 
  let num = List.length names in
  let idx = Random.self_init (); Random.int num in
  List.nth names idx
