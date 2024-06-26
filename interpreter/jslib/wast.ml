(* Implements a wrapper library that allows the use of the reference
 * interpreter's encode/decode functionality in JavaScript.
 *)
open Wasm
open Js_of_ocaml

let () =
  Js.export "WebAssemblyText"
    (object%js (_self)

      method encode (s : Js.js_string Js.t) : (Typed_array.arrayBuffer Js.t) =
        let _, def = Parse.Module.parse_string (Js.to_string s) in
        let bs =
          match def.Source.it with
          | Script.Textual (m, cs) -> Encode.encode_with_custom (m, cs)
          | Script.Encoded (_, bs) -> bs.Source.it
          | Script.Quoted (_, _) -> failwith "Unsupported" in
        let buf = new%js Typed_array.arrayBuffer (String.length bs) in
        let u8arr = new%js Typed_array.uint8Array_fromBuffer buf in
        String.iteri (fun i c -> Typed_array.set u8arr i (int_of_char c)) bs; buf

      method decode (buf : Typed_array.arrayBuffer Js.t) width : (Js.js_string Js.t) =
        let s = Typed_array.String.of_uint8Array (new%js Typed_array.uint8Array_fromBuffer buf) in
        let m = Decode.decode "(decode)" s in
        Js.string (Sexpr.to_string width (Arrange.module_ m))

    end)
