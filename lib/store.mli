open Ast

type store = {
    ids : identifier list;
    values : int list;
  }


(** Creates an initial store from program declaration identifier list **)
val create : identifier list -> store

val get_values : store -> int list

val get_ids: store -> identifier list

val get_value : store -> identifier -> int

val remove_pair: store -> identifier -> store

val set_value : store -> identifier -> int -> store

(** Converts the given global variable store to a string *)
val string_of_store   : store   -> string