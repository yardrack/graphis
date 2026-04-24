module Heap = Heap
module Dot = Dot

type context = Heap.context
type direct = Heap.direct =
  | Immediate of int
  | Pointer of Heap.pointer

let context = Heap.context
let capture = Heap.capture
let repr = Heap.repr
let print_dot = Dot.print
let write_dot = Dot.to_file
