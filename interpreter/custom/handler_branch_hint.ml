(* Handler for "metadata.code.branch_hint" section and @metadata.code.branch_hint annotations *)

open Custom
open Annot
open Source
open Ast

module IdxMap = Map.Make(Int32)

type kind = Likely | Unlikely
type hint = kind Source.phrase
type hints = hint list
type func_hints = hints IdxMap.t

type format = format' Source.phrase
and format' =
{
  func_hints: func_hints;
  gaps: (int * Source.region * Source.region) list ref;
}


let empty = {func_hints = IdxMap.empty; gaps = ref [] }

let name = Utf8.decode "metadata.code.branch_hint"

let place _fmt = Before Code

let is_contained r1 r2 = r1.left >= r2.left && r1.right <= r2.right
let is_left r1 r2 = r1.right <= r2.left

let rec find_inst' i at =
  if is_left at i.at then
    Some i
  else match i.it with
  | Block (bt, es) -> find_inst es at
  | Loop (bt, es) -> find_inst es at
  | If (bt, es, es') ->
      (match find_inst es at with
      | Some i -> Some i
      | None -> find_inst es' at)
  | _ -> None

and find_inst es at = match es with
| [] -> None
| e::es ->
    match find_inst' e at with
    | None -> find_inst es at
    | Some i -> Some i


(* Decoding *)

(* TODO: make Decode module reusable instead of duplicating code *)

type stream = {bytes : string; pos : int ref}

exception EOS

let stream bs = {bytes = bs; pos = ref 0}

let len s = String.length s.bytes
let pos s = !(s.pos)

let check n s = if pos s + n > len s then raise EOS
let skip n s = if n < 0 then raise EOS else check n s; s.pos := !(s.pos) + n

let read s = Char.code (s.bytes.[!(s.pos)])
let get s = check 1 s; let b = read s in skip 1 s; b

let position file pos = Source.{file; line = -1; column = pos}
let region file left right = Source.{left = position file left; right = position file right}

let decode_error pos msg = raise (Custom.Code (region "@metadata.code.branch_hint section" pos pos, msg))
let require b pos msg = if not b then decode_error pos msg

let decode_byte s =
  get s

let rec decode_uN n s =
  require (n > 0) (pos s) "integer representation too long";
  let b = decode_byte s in
  require (n >= 7 || b land 0x7f < 1 lsl n) (pos s - 1) "integer too large";
  let x = Int32.of_int (b land 0x7f) in
  if b land 0x80 = 0 then x else
  Int32.(logor x (shift_left (decode_uN (n - 7) s) 7))

let decode_u32 = decode_uN 32

let decode_vec f s =
  let n = decode_u32 s in
  let n = Int32.to_int n in
  let rec it i s =
    if i = 0 then
      []
    else
      [f s] @ it (i - 1) s
  in
  it n s

let decode_hint file foff s =
  let off = decode_u32 s in
  let one = decode_u32 s in
  require (one = 1l) (pos s) "missing required 0x01 byte";
  let k = decode_byte s in
  let hint = match k with
  | 0x00 -> Unlikely
  | 0x01 -> Likely
  | _ -> decode_error (pos s) "Unexpected hint value" in
  let abs_off = Int32.to_int (Int32.add off foff) in
  hint @@ region file abs_off abs_off

let decode_func_hints file foff =
  decode_vec (decode_hint file foff)

let get_func m fidx =
  let nimp = List.length m.it.imports in
  let fn = (Int32.to_int fidx) - nimp in
  List.nth m.it.funcs fn

let decode_func m s =
  let f = decode_u32 s in
  let fat = (get_func m f).at in
  let foff = Int32.of_int fat.left.column in
  let file = fat.left.file in 
  let hs = decode_func_hints file foff s in
  (f, hs)

let decode_funcs m s =
  let fs = decode_vec (decode_func m) s in
  IdxMap.add_seq (List.to_seq fs) IdxMap.empty

let decode m _ custom =
  let s = stream custom.it.content in
  try
    { func_hints = decode_funcs m s; gaps = ref [] } @@ custom.at
  with EOS -> decode_error (pos s) "unexpected end of name section"


(* Encoding *)

(* TODO: make Encode module reusable *)

let encode_byte buf b =
  Buffer.add_char buf (Char.chr b)

let rec encode_u32 buf i =
  let b = Int32.(to_int (logand i 0x7fl)) in
  if 0l <= i && i < 128l then encode_byte buf b
  else (
    encode_byte buf (b lor 0x80);
    encode_u32 buf (Int32.shift_right_logical i 7)
  )

let pos buf = Buffer.length buf
let gap32 buf = let p = pos buf in
  encode_byte buf 0xfe;
  encode_byte buf 0xfe;
  encode_byte buf 0xfe;
  encode_byte buf 0xfe;
  encode_byte buf 0xfe;
  p
  
let patch_gap32 p n =
  assert (n <= 0x0fff_ffff); (* Strings cannot excess 2G anyway *)
  let patch ps pos b = (pos, b) :: ps in
  let lsb i = Char.chr (i land 0xff) in
  let ps = patch [] p (lsb (n lor 0x80)) in
  let ps = patch ps (p + 1) (lsb ((n lsr 7) lor 0x80)) in
  let ps = patch ps (p + 2) (lsb ((n lsr 14) lor 0x80)) in
  let ps = patch ps (p + 3) (lsb ((n lsr 21) lor 0x80)) in
  let ps = patch ps (p + 4) (lsb (n lsr 28)) in
  ps

let encode_size buf n =
  encode_u32 buf (Int32.of_int n)

let encode_vec buf f v =
  encode_size buf (List.length v);
  let rec it v = match v with
  | [] -> ()
  | e::es -> f buf e; it es in
  it v

let encode_hint gaps func buf h =
  let g = gap32 buf in
  let i = Option.get (find_inst func.it.body h.at) in
  gaps := (g, func.at, i.at) :: !gaps;
  encode_u32 buf 1l;
  let b = match h.it with
  | Unlikely -> 0l
  | Likely -> 1l in
  encode_u32 buf b

let encode_func_hints m gaps buf f =
  let fn = (Int32.to_int f) - (List.length m.it.imports) in
  let func = List.nth m.it.funcs fn in
  encode_vec buf (encode_hint gaps func)

let encode_func m gaps buf t =
  let f, hs = t in
  encode_u32 buf f;
  encode_func_hints m gaps buf f hs

let encode_funcs m gaps buf fhs =
  encode_vec buf (encode_func m gaps) (List.of_seq (IdxMap.to_seq fhs))

let encode m _bs sec =
  let {func_hints; gaps} = sec.it in
  let buf = Buffer.create 200 in
  encode_funcs m gaps buf func_hints;
  let content = Buffer.contents buf in
  {name = Utf8.decode "metadata.code.branch_hint"; content; place = Before Code} @@ sec.at


let patch_one ros off gap =
  let pos, fat, hat = gap in
  let foff = Hashtbl.find ros fat in
  let hoff = Hashtbl.find ros hat in
  let p = hoff - foff in
  patch_gap32 (pos+off) p

let patch _m f cp =
  let gaps = !(f.it.gaps) in
  let off = cp.custom_offset in
  let ros = cp.region_offsets in
  List.concat_map (patch_one ros off) gaps

(* Parsing *)

open Ast

let parse_error at msg = raise (Custom.Syntax (at, msg))

let merge_func_hints = IdxMap.merge (fun key x y ->
  match x, y with
  | Some a, None -> Some a
  | None, Some b -> Some b
  | Some a, Some b -> Some (a @ b)
  | None, None -> None )

let merge s1 s2 =
  {
    func_hints = merge_func_hints s1.it.func_hints s2.it.func_hints;
    gaps = ref (!(s1.it.gaps) @ !(s2.it.gaps))
  } @@ {left = s1.at.left; right = s2.at.right}

let find_func_idx m annot =
  let idx = Lib.List.index_where (fun f -> is_contained annot.at f.at ) m.it.funcs in
  match idx with
  | Some i -> Int32.of_int (i + List.length m.it.imports)
  | None -> parse_error annot.at "Branch hint annotation is not in a function"

let rec parse m _bs annots =
  let ms = List.map (parse_annot m) annots in
  match ms with
  | [] -> []
  | m::ms' -> [List.fold_left merge (empty @@ m.at) ms]

and parse_annot m annot =
  let {name = n; items} = annot.it in
  assert (n = name);
  let payload a = match a.it with
    | String s -> s
    | _ -> parse_error a.at "Unexpected token" in
  let fold_payload bs a = bs ^ (payload a) in
  let p = List.fold_left fold_payload "" items in
  let at_last = (List.hd (List.rev items)).at in
  let f = find_func_idx m annot in
  let hint = match p with
  | "\x00" -> Unlikely
  | "\x01" -> Likely
  | _ -> parse_error annot.at ("Branch hint has an invalid value" ^ p) in
  let e = { func_hints = IdxMap.add f [hint @@ annot.at] IdxMap.empty; gaps = ref [] } in
  e @@ Source.{left = annot.at.left; right = at_last.right}

(* Printing *)

let arrange m fmt =
  let hint_to_string = function
    | Likely -> "\"\\01\""
    | Unlikely -> "\"\\00\"" in

  let arrange_one h =
    (Sexpr.Node ("@metadata.code.branch_hint ", [Sexpr.Atom (hint_to_string h.it)])) @@ h.at in 
  let arrange_func (f, hs) =
    List.map arrange_one hs in
  let arrange_funcs (fhs) =
    List.concat (List.map arrange_func fhs) in
  CodeAnnot (arrange_funcs (List.of_seq (IdxMap.to_seq fmt.it.func_hints)))




(* Checking *)

let check_error at msg = raise (Custom.Invalid (at, msg))

let check_one m f h =
  let fn = (Int32.to_int f) - (List.length m.imports) in
  let es = (List.nth m.funcs fn).it.body in
  match find_inst es h.at with
  | None -> check_error h.at "@metadata.code.branch_hint annotation: invalid offset"
  | Some i ->
      (match i.it with
      | If _ -> ()
      | BrIf _ -> ()
      | _ -> check_error h.at "@metadata.code.branch_hint annotation: invalid target")


let check (m : module_) (fmt : format) =
  IdxMap.iter (fun f hs -> List.iter (fun h -> check_one m.it f h) hs) fmt.it.func_hints;
  ()

