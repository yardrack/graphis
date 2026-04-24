type tree =
  | Leaf of int
  | Node of tree * tree

let int_value = 13
let float_value = 42.0
let list_value = [ 1; 2; 3 ]
let array_value = [| "a"; "b"; "c" |]
let float_array = [| 1.0; 2.0; 3.0 |]
let shared = Leaf 7
let tree = Node (shared, shared)
let rec cycle = 1 :: 2 :: 3 :: cycle

let () =
  Graphis.context (fun ctx ->
      Graphis.print_dot Format.std_formatter
        [
          "cycle", Graphis.capture ctx cycle;
          "tree", Graphis.capture ctx tree;
          "float_array", Graphis.capture ctx float_array;
          "array", Graphis.capture ctx array_value;
          "list", Graphis.capture ctx list_value;
          "float", Graphis.capture ctx float_value;
          "int", Graphis.capture ctx int_value;
        ])
