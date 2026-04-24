type node = {
  value : int;
  mutable next : node option;
}

let node value = { value; next = None }

let () =
  let tail = node 30 in
  let left = node 10 in
  let right = node 20 in
  left.next <- Some tail;
  right.next <- Some tail;
  tail.next <- Some left;

  Graphis.context (fun ctx ->
      Graphis.print_dot Format.std_formatter
        [
          "left_head", Graphis.capture ctx left;
          "right_head", Graphis.capture ctx right;
          "shared_tail", Graphis.capture ctx tail;
        ])
