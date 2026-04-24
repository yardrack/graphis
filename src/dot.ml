type config = {
  external_color : string option;
  block_color : string option;
  root_color : string option;
  outline_color : string;
  background : string option;
  direction : [ `Vertical | `Horizontal ];
}

let config
    ?(external_color = Some "gray86")
    ?(block_color = Some "aliceblue")
    ?(root_color = Some "khaki1")
    ?(outline_color = "gray20")
    ?(background = None)
    ?(direction = `Vertical)
    () =
  { external_color; block_color; root_color; outline_color; background; direction }

let escape_label text =
  let b = Buffer.create (String.length text + 8) in
  String.iter
    (function
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '{' -> Buffer.add_string b "\\{"
      | '}' -> Buffer.add_string b "\\}"
      | '|' -> Buffer.add_string b "\\|"
      | '<' -> Buffer.add_string b "\\<"
      | '>' -> Buffer.add_string b "\\>"
      | '\n' -> Buffer.add_string b "\\n"
      | c -> Buffer.add_char b c)
    text;
  Buffer.contents b

let style color fmt extras =
  let styles = match color with None -> extras | Some _ -> "filled" :: extras in
  begin
    match styles with
    | [] -> ()
    | _ -> Format.fprintf fmt " style=\"%s\"" (String.concat "," styles)
  end;
  match color with
  | None -> ()
  | Some color -> Format.fprintf fmt " fillcolor=\"%s\"" (escape_label color)

let graph_prefix graph = Format.asprintf "g%d" (Heap.graph_id graph)
let node_name graph addr = Format.asprintf "%s_n%d" (graph_prefix graph) (Heap.addr_int addr)
let external_name graph value =
  let raw = Format.asprintf "%nx" value in
  let clean =
    String.map
      (function
        | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' as c -> c
        | '-' -> 'm'
        | _ -> '_')
      raw
  in
  Format.asprintf "%s_e%s" (graph_prefix graph) clean

let root_name index = Format.asprintf "root_%d" index

let node_port item =
  match item.Heap.offset with
  | 0 -> "head"
  | offset -> Format.asprintf "f%d" (offset - 1)

let direct_text = function
  | Heap.Immediate i -> string_of_int i
  | Heap.Pointer _ -> "."

let slot_text = function
  | Heap.Slot_int i -> string_of_int i
  | Slot_ptr _ -> "."
  | Slot_external _ -> "."
  | Slot_float f -> Printf.sprintf "%g" f
  | Slot_infix -> "infix"
  | Slot_closure_info info ->
      Printf.sprintf "arity:%d env:%d" info.arity info.env_start

let payload_label = function
  | Heap.Opaque -> "<f0> opaque"
  | String_data text -> Format.asprintf "<f0> string:%s" (escape_label text)
  | Float_data f -> Format.asprintf "<f0> %g" f
  | Slots [||] -> ""
  | Slots slots ->
      Array.mapi
        (fun i slot -> Format.asprintf "<f%d> %s" i (escape_label (slot_text slot)))
        slots
      |> Array.to_list
      |> String.concat " | "

let node_label item =
  Format.asprintf "{ <head> tag:%d | %s }"
    (Heap.tag_int item.Heap.node.tag)
    (payload_label item.Heap.node.payload)

let print_external config printed fmt graph value =
  let name = external_name graph value in
  if not (Hashtbl.mem printed name) then begin
    Hashtbl.add printed name ();
    Format.fprintf fmt "%s [label=\"{ <head> external | 0x%nx }\" shape=\"record\"%a];@\n"
      name value (style config.external_color) [ "rounded" ]
  end

let print_edges config printed_external fmt item =
  match item.Heap.node.payload with
  | Opaque | String_data _ | Float_data _ -> ()
  | Slots slots ->
      Array.iteri
        (fun index -> function
          | Heap.Slot_ptr addr ->
              let dst = Heap.follow_addr item.graph addr in
              Format.fprintf fmt "%s:f%d -> %s:%s;@\n"
                (node_name item.graph item.node.addr)
                index
                (node_name item.graph dst.node.addr)
                (node_port dst)
          | Slot_external value ->
              print_external config printed_external fmt item.graph value;
              Format.fprintf fmt "%s:f%d -> %s:head;@\n"
                (node_name item.graph item.node.addr)
                index
                (external_name item.graph value)
          | _ -> ())
        slots

let print_node config printed_nodes printed_external fmt item =
  let name = node_name item.Heap.graph item.node.addr in
  if not (Hashtbl.mem printed_nodes name) then begin
    Hashtbl.add printed_nodes name ();
    Format.fprintf fmt "%s [label=\"%s\" shape=\"record\"%a];@\n"
      name
      (node_label item)
      (style config.block_color) [ "rounded" ];
    print_edges config printed_external fmt item
  end

let print_root config fmt index (label, direct) =
  Format.fprintf fmt "%s [label=\"{ value:%s | %s }\" shape=\"record\"%a];@\n"
    (root_name index)
    (escape_label label)
    (escape_label (direct_text direct))
    (style config.root_color) []

let print_root_edge fmt index = function
  | _, Heap.Immediate _ -> ()
  | _, Pointer pointer ->
      let item = Heap.follow pointer in
      Format.fprintf fmt "%s -> %s:%s;@\n"
        (root_name index)
        (node_name pointer.graph item.node.addr)
        (node_port item)

let print ?(config = config ()) fmt roots =
  let printed_nodes = Hashtbl.create 64 in
  let printed_external = Hashtbl.create 32 in
  Format.fprintf fmt "digraph graphis {@\n";
  Format.fprintf fmt "graph [bgcolor=\"%s\"];@\n"
    (match config.background with None -> "transparent" | Some c -> escape_label c);
  Format.fprintf fmt "edge [color=\"%s\"];@\n" (escape_label config.outline_color);
  Format.fprintf fmt "node [color=\"%s\", fontcolor=\"%s\"];@\n"
    (escape_label config.outline_color)
    (escape_label config.outline_color);
  Format.fprintf fmt "rankdir=%s;@\n"
    (match config.direction with `Vertical -> "TB" | `Horizontal -> "LR");
  Format.fprintf fmt "{ rank=source;@\n";
  List.iteri (print_root config fmt) roots;
  Format.fprintf fmt "}@\n";
  List.iteri (print_root_edge fmt) roots;
  List.iter
    (function
      | _, Heap.Immediate _ -> ()
      | _, Pointer pointer ->
          Heap.walk
            (print_node config printed_nodes printed_external fmt)
            (Heap.follow pointer))
    roots;
  Format.fprintf fmt "}@."

let to_file ?config path roots =
  let flags = [ Unix.O_CREAT; Unix.O_EXCL; Unix.O_WRONLY ] in
  let fd = Unix.openfile path flags 0o640 in
  let channel = Unix.out_channel_of_descr fd in
  Fun.protect
    ~finally:(fun () -> close_out channel)
    (fun () ->
      let fmt = Format.formatter_of_out_channel channel in
      print ?config fmt roots)
