let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let () =
  match Sys.argv with
  | [| _ |] ->
    Printf.eprintf "Usage: p4-project <file>\n";
    exit 1
  | [| _; filename |] ->
    let source = read_file filename in
    if String.length (String.trim source) = 0 then begin
      Printf.eprintf "Error: '%s' is empty\n" filename;
      exit 1
    end;
    (* TODO: lex, parse, and execute source *)
    ignore source
  | _ ->
    Printf.eprintf "Usage: p4-project <file>\n";
    exit 1
