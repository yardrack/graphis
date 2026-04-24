type tag = int
type addr = int

type closure_info = {
  arity : int;
  env_start : int;
}

type graph = {
  id : int;
  mutable next_addr : int;
  blocks : (addr, pointee) Hashtbl.t;
}

and pointer = {
  graph : graph;
  addr : addr;
}

and direct =
  | Immediate of int
  | Pointer of pointer

and slot =
  | Slot_int of int
  | Slot_ptr of addr
  | Slot_external of nativeint
  | Slot_float of float
  | Slot_infix
  | Slot_closure_info of closure_info

and payload =
  | Opaque
  | String_data of string
  | Float_data of float
  | Slots of slot array

and node = {
  addr : addr;
  tag : tag;
  payload : payload;
}

and pointee = {
  graph : graph;
  node : node;
  offset : int;
}

type context = {
  graph : graph;
  mutable seen : (Obj.t * addr) list;
}

type lookup_error = Missing_addr of { graph_id : int; addr : addr }
exception Lookup_error of lookup_error

type validation_error =
  | Missing_block of addr
  | Node_addr_mismatch of { table_addr : addr; node_addr : addr }
  | Dangling_slot of { from_addr : addr; slot : int; target_addr : addr }

let graph_seq = ref 0

let fresh_graph () =
  incr graph_seq;
  { id = !graph_seq; next_addr = 0; blocks = Hashtbl.create 64 }

let fresh_addr graph =
  graph.next_addr <- graph.next_addr + 1;
  graph.next_addr

let graph_id graph = graph.id
let addr_int addr = addr
let tag_int tag = tag

let find_addr (graph : graph) addr =
  match Hashtbl.find_opt graph.blocks addr with
  | Some item -> Ok item
  | None -> Error (Missing_addr { graph_id = graph.id; addr })

let follow_addr (graph : graph) addr =
  match find_addr graph addr with
  | Ok item -> item
  | Error error -> raise (Lookup_error error)

let follow (pointer : pointer) = follow_addr pointer.graph pointer.addr

let walk visit root =
  let visited = Hashtbl.create 64 in
  let pending = Stack.create () in
  Stack.push root.node.addr pending;
  while not (Stack.is_empty pending) do
    let addr = Stack.pop pending in
    if not (Hashtbl.mem visited addr) then begin
      Hashtbl.add visited addr ();
      let item = follow_addr root.graph addr in
      visit item;
      match item.node.payload with
      | Opaque | String_data _ | Float_data _ -> ()
      | Slots slots ->
          Array.iter
            (function
              | Slot_ptr addr when not (Hashtbl.mem visited addr) ->
                  Stack.push addr pending
              | _ -> ())
            slots
    end
  done

let validate graph =
  let errors = ref [] in
  let require_block addr =
    if not (Hashtbl.mem graph.blocks addr) then
      errors := Missing_block addr :: !errors
  in
  Hashtbl.iter
    (fun table_addr item ->
      if item.offset = 0 && item.node.addr <> table_addr then
        errors :=
          Node_addr_mismatch { table_addr; node_addr = item.node.addr } :: !errors;
      match item.node.payload with
      | Opaque | String_data _ | Float_data _ -> ()
      | Slots slots ->
          Array.iteri
            (fun slot -> function
              | Slot_ptr target_addr ->
                  if not (Hashtbl.mem graph.blocks target_addr) then
                    errors :=
                      Dangling_slot
                        { from_addr = item.node.addr; slot; target_addr }
                      :: !errors
              | _ -> ())
            slots)
    graph.blocks;
  for addr = 1 to graph.next_addr do
    require_block addr
  done;
  match List.rev !errors with
  | [] -> Ok ()
  | errors -> Error errors

let is_env_start info next size =
  info.env_start = next
  && next <= size
  && ((info.arity = 1 && info.env_start = 2)
     || (info.arity > 1 && info.env_start = 3))

let closure_info block index =
  let raw = Obj.field block index in
  if not (Obj.is_int raw) then { arity = 0; env_start = max_int }
  else
    let packed : int = Obj.obj raw in
    let arity = packed lsr (Sys.word_size - 9) in
    let env_start = (packed lsl 8) lsr 8 in
    { arity; env_start }

let rec lower_block ctx addr value =
  let tag = Obj.tag value in
  if tag = Obj.infix_tag then
    lower_infix ctx addr value
  else
    let payload, seen =
      if tag = Obj.double_tag then
        Float_data (Obj.obj value : float), ctx.seen
      else if tag = Obj.string_tag then
        String_data (Obj.obj value : string), ctx.seen
      else if tag = Obj.double_array_tag then
        let slots =
          Array.init (Obj.size value) (fun index ->
              Slot_float (Obj.double_field value index))
        in
        Slots slots, ctx.seen
      else if tag = Obj.closure_tag then
        lower_closure ctx value
      else if tag < Obj.no_scan_tag then
        let seen = ref ctx.seen in
        let slots =
          Array.init (Obj.size value) (fun index ->
              let next, slot = lower_slot ctx !seen (Obj.field value index) in
              seen := next;
              slot)
        in
        Slots slots, !seen
      else
        Opaque, ctx.seen
    in
    let node = { addr; tag; payload } in
    Hashtbl.replace ctx.graph.blocks addr { graph = ctx.graph; node; offset = 0 };
    ctx.seen <- (value, addr) :: seen

and lower_infix ctx addr value =
  let offset = Obj.size value in
  let bytes = offset * Sys.word_size / 8 in
  let base = Obj.add_offset value Int32.(neg (of_int bytes)) in
  let seen, direct = lower_direct ctx ctx.seen base in
  ctx.seen <- seen;
  match direct with
  | Immediate _ -> assert false
  | Pointer parent ->
      let target = follow parent in
      begin
        match target.node.payload with
        | Slots slots when offset > 0 && offset - 1 < Array.length slots ->
            slots.(offset - 1) <- Slot_infix
        | _ -> ()
      end;
      Hashtbl.replace ctx.graph.blocks addr
        { graph = ctx.graph; node = target.node; offset };
      ctx.seen <- (value, addr) :: ctx.seen

and lower_closure ctx value =
  match Sys.backend_type with
  | Sys.Native | Sys.Bytecode ->
      let size = Obj.size value in
      let seen, slots = closure_entries ctx ctx.seen value size 0 [] in
      Slots (Array.of_list (List.rev slots)), seen
  | Sys.Other _ -> Opaque, ctx.seen

and closure_entries ctx seen value size index acc =
  if index >= size then
    seen, acc
  else
    let index, acc =
      if index = 0 then
        index, acc
      else
        index + 1, Slot_infix :: acc
    in
    if index + 1 >= size then
      seen, acc
    else
      let info = closure_info value (index + 1) in
      let code = Slot_external (Obj.raw_field value index) in
      let acc, next =
        if info.arity = 1 || index + 2 >= size then
          Slot_closure_info info :: code :: acc, index + 2
        else
          let curry = Slot_external (Obj.raw_field value (index + 2)) in
          curry :: Slot_closure_info info :: code :: acc, index + 3
      in
      if is_env_start info next size then
        closure_env ctx seen value size next acc
      else
        closure_entries ctx seen value size next acc

and closure_env ctx seen value size index acc =
  if index >= size then
    seen, acc
  else
    let seen, slot = lower_slot ctx seen (Obj.field value index) in
    closure_env ctx seen value size (index + 1) (slot :: acc)

and lower_slot ctx seen value =
  if Obj.is_int value then
    seen, Slot_int (Obj.obj value : int)
  else if Obj.tag value = Obj.out_of_heap_tag then
    let raw : int = Obj.magic value in
    seen, Slot_external Nativeint.(shift_left (of_int raw) 1)
  else
    match List.assq_opt value seen with
    | Some addr -> seen, Slot_ptr addr
    | None ->
        let addr = fresh_addr ctx.graph in
        ctx.seen <- (value, addr) :: seen;
        lower_block ctx addr value;
        ctx.seen, Slot_ptr addr

and lower_direct ctx seen value =
  if Obj.is_int value then
    seen, Immediate (Obj.obj value : int)
  else
    match List.assq_opt value seen with
    | Some addr -> seen, Pointer { graph = ctx.graph; addr }
    | None ->
        let addr = fresh_addr ctx.graph in
        ctx.seen <- (value, addr) :: seen;
        lower_block ctx addr value;
        ctx.seen, Pointer { graph = ctx.graph; addr }

let context f =
  let ctx = { graph = fresh_graph (); seen = [] } in
  f ctx

let capture ctx value =
  let seen, direct = lower_direct ctx ctx.seen (Obj.repr value) in
  ctx.seen <- seen;
  direct

let repr value = context (fun ctx -> capture ctx value)
