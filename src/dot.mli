(** Graphviz DOT lowering. *)

type config

val config :
  ?external_color:string option ->
  ?block_color:string option ->
  ?root_color:string option ->
  ?outline_color:string ->
  ?background:string option ->
  ?direction:[ `Vertical | `Horizontal ] ->
  unit ->
  config

val print : ?config:config -> Format.formatter -> (string * Heap.direct) list -> unit
val to_file : ?config:config -> string -> (string * Heap.direct) list -> unit
