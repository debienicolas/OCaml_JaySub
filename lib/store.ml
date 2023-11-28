open Ast


(* type store = (identifier * int) list *)

type store = {
  ids : identifier list;
  values : int list;
}


(* Creates an initial store that has the identifiers with values set to O given a program input*)
let rec create (input : identifier list) : store = match input with
  | [] ->  { ids = []; values = [] }
  | h::t -> let rest = create t in { 
                ids = h::rest.ids; 
                values = 0::rest.values }

let get_values (s:store) : int list = s.values
let get_ids (s:store) : identifier list = s.ids

let rec get_value (s:store) (id:identifier) : int = match s.ids with
  | [] -> failwith "Identifier not found"
  | h::t -> if h = id then List.hd s.values else get_value { ids = t; values = List.tl s.values } id

let rec remove_pair (s:store) (id:identifier) : store = match s.ids with
  | [] -> failwith "Identifier not found"
  | h::t -> if h = id 
            then {ids = t; values = List.tl s.values}
            else {ids = h::(remove_pair {ids = t; values = List.tl s.values} id).ids; values = List.hd s.values::(remove_pair {ids = t; values = List.tl s.values} id).values}
  
let set_value (s:store) (id:identifier) (value:int) : store = 
  let s' = remove_pair s id in
  {ids = id::s'.ids; values = value::s'.values}


let string_of_store store =
  let rec helper ids values acc =
    match ids, values with
    | [], [] -> acc
    | id :: ids_tail, value :: values_tail ->
        let pair_str = Printf.sprintf "%s -> %d; " id value in
        helper ids_tail values_tail (acc ^ pair_str)
    | _, _ -> failwith "Mismatched ids and values lengths in store"
  in
  helper store.ids store.values ""



