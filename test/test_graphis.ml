let assert_true message condition =
  if not condition then failwith message

let assert_equal_int message expected actual =
  if expected <> actual then
    failwith
      (Printf.sprintf "%s: expected %d, got %d" message expected actual)

let dot roots =
  Format.asprintf "%a" (Graphis.print_dot ?config:None) roots

let contains needle haystack =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec loop index =
    index + needle_len <= haystack_len
    && (String.sub haystack index needle_len = needle || loop (index + 1))
  in
  needle_len = 0 || loop 0

let pointer message = function
  | Graphis.Pointer pointer -> pointer
  | Immediate _ -> failwith message

let follow_direct message direct =
  Graphis.Heap.follow (pointer message direct)

let validate_direct direct =
  let pointer = pointer "expected pointer root" direct in
  match Graphis.Heap.validate pointer.graph with
  | Ok () -> ()
  | Error _ -> failwith "heap graph should validate"

let reachable_count item =
  let count = ref 0 in
  Graphis.Heap.walk (fun _ -> incr count) item;
  !count

let slot_ptrs item =
  match item.Graphis.Heap.node.payload with
  | Slots slots ->
      Array.to_list slots
      |> List.filter_map (function
           | Graphis.Heap.Slot_ptr addr -> Some addr
           | _ -> None)
  | _ -> []

let test_immediate () =
  match Graphis.repr 41 with
  | Immediate 41 -> ()
  | _ -> failwith "int should lower to immediate"

let test_sharing () =
  let pair =
    Graphis.context (fun ctx ->
        let shared = [ 1; 2 ] in
        Graphis.capture ctx (shared, shared))
  in
  validate_direct pair;
  let item = follow_direct "pair should be a pointer" pair in
  match slot_ptrs item with
  | left :: right :: _ ->
      assert_true "shared pair should preserve physical aliasing" (left = right)
  | _ -> failwith "pair should lower to pointer slots"

let test_cycle () =
  let root =
    Graphis.context (fun ctx ->
        let rec xs = 1 :: 2 :: xs in
        Graphis.capture ctx xs)
  in
  validate_direct root;
  let item = follow_direct "cycle should be a pointer" root in
  assert_equal_int "cycle should contain two cons cells" 2 (reachable_count item)

let test_strings_and_floats () =
  let root =
    Graphis.context (fun ctx ->
        Graphis.capture ctx ("alpha", [| 1.0; 2.0 |]))
  in
  validate_direct root;
  let output = dot [ "mixed", root ] in
  assert_true "string payload should be visible" (contains "alpha" output);
  assert_true "float array payload should be visible" (contains "1" output)

let test_opaque_block () =
  let root = Graphis.repr stdin in
  validate_direct root;
  let item = follow_direct "channel should be a pointer" root in
  assert_true "channel should lower to an opaque runtime block"
    (match item.node.payload with Graphis.Heap.Opaque -> true | _ -> false)

let test_closure () =
  let root =
    Graphis.context (fun ctx ->
        let base = 3 in
        Graphis.capture ctx (fun x -> x + base))
  in
  validate_direct root;
  let output = dot [ "fn", root ] in
  assert_true "closure should expose closure metadata"
    (contains "arity:" output || contains "opaque" output)

let test_dot_escaping_and_config () =
  let root = Graphis.repr ("a\"b\\c{d}|<e>\nf") in
  let config =
    Graphis.Dot.config ~background:(Some "white") ~direction:`Horizontal ()
  in
  let output = Format.asprintf "%a" (Graphis.print_dot ~config) [ "s", root ] in
  assert_true "DOT should escape quotes" (contains "\\\"" output);
  assert_true "DOT should escape backslashes" (contains "\\\\" output);
  assert_true "DOT should escape record separators" (contains "\\|" output);
  assert_true "DOT should apply horizontal rankdir" (contains "rankdir=LR" output);
  assert_true "DOT should apply background" (contains "bgcolor=\"white\"" output)

let test_root_ordering () =
  let output = dot [ "a", Graphis.repr 1; "b", Graphis.repr 2 ] in
  assert_true "first root should be emitted" (contains "root_0" output);
  assert_true "second root should be emitted" (contains "root_1" output)

let () =
  test_immediate ();
  test_sharing ();
  test_cycle ();
  test_strings_and_floats ();
  test_opaque_block ();
  test_closure ();
  test_dot_escaping_and_config ();
  test_root_ordering ()
