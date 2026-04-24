module Heap = Heap
module Dot = Dot

type context = Heap.context
type direct = Heap.direct =
  | Immediate of int
  | Pointer of Heap.pointer

val context : (context -> 'a) -> 'a
val capture : context -> 'a -> direct
val repr : 'a -> direct

val print_dot :
  ?config:Dot.config ->
  Format.formatter ->
  (string * direct) list ->
  unit

val write_dot : ?config:Dot.config -> string -> (string * direct) list -> unit
