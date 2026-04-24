let measure name make =
  Gc.compact ();
  let start = Unix.gettimeofday () in
  let root = Graphis.repr (make ()) in
  let elapsed = Unix.gettimeofday () -. start in
  let blocks =
    match root with
    | Graphis.Immediate _ -> 0
    | Pointer pointer ->
        let count = ref 0 in
        Graphis.Heap.walk (fun _ -> incr count) (Graphis.Heap.follow pointer);
        !count
  in
  Printf.printf "%-12s %8d blocks %.6fs\n%!" name blocks elapsed

type tree =
  | Leaf of int
  | Node of tree * tree

let rec tree depth =
  if depth = 0 then Leaf depth else Node (tree (depth - 1), tree (depth - 1))

let list size = List.init size Fun.id

let closure_chain size =
  let rec loop index acc =
    if index = size then acc else loop (index + 1) (fun x -> acc (x + index))
  in
  loop 0 Fun.id

type node = {
  value : int;
  mutable next : node option;
}

let cycle size =
  let first = { value = 0; next = None } in
  let rec loop prev index =
    if index = size then
      prev.next <- Some first
    else
      let node = { value = index; next = None } in
      prev.next <- Some node;
      loop node (index + 1)
  in
  loop first 1;
  first

let () =
  measure "list" (fun () -> list 10_000);
  measure "tree" (fun () -> tree 12);
  measure "closures" (fun () -> closure_chain 1_000);
  measure "cycle" (fun () -> cycle 10_000)
