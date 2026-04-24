(** this module is the unsafe boundary. it reads OCaml runtime values through
    [Obj], preserves physical sharing and then  assigns deterministic graph-local
    addresses to heap blocks. *)

type tag = private int
type addr = private int

type closure_info = {
  arity : int;
  env_start : int;
}

type graph

type pointer = private {
  graph : graph;
  addr : addr;
}

type direct =
  | Immediate of int
  | Pointer of pointer

type slot =
  | Slot_int of int
  | Slot_ptr of addr
  | Slot_external of nativeint
  | Slot_float of float
  | Slot_infix
  | Slot_closure_info of closure_info

type payload =
  | Opaque
  | String_data of string
  | Float_data of float
  | Slots of slot array

type node = private {
  addr : addr;
  tag : tag;
  payload : payload;
}

type pointee = private {
  graph : graph;
  node : node;
  offset : int;
}

type lookup_error = Missing_addr of { graph_id : int; addr : addr }
exception Lookup_error of lookup_error

type validation_error =
  | Missing_block of addr
  | Node_addr_mismatch of { table_addr : addr; node_addr : addr }
  | Dangling_slot of { from_addr : addr; slot : int; target_addr : addr }

type context

val context : (context -> 'a) -> 'a
(** run a capture scope. values captured with the supplied context share one
    graph, so physical sharing across roots is preserved. *)

val capture : context -> 'a -> direct
val repr : 'a -> direct

val follow : pointer -> pointee
val follow_addr : graph -> addr -> pointee
(** [follow_addr graph addr] resolves [addr] or raises [Lookup_error]. *)

val find_addr : graph -> addr -> (pointee, lookup_error) result
val walk : (pointee -> unit) -> pointee -> unit
(** depth-first traversal over blocks reachable from the supplied pointee.
    each graph address is visited at most once. *)

val validate : graph -> (unit, validation_error list) result

val graph_id : graph -> int
val addr_int : addr -> int
val tag_int : tag -> int
