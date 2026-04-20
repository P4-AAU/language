(* Code generation: AST -> C source *)

open Ast

let rec collect_vars_stmt acc = function
  | Sassign (id, _) -> if List.mem id.id acc then acc else id.id :: acc
  | Sblock stmts -> List.fold_left collect_vars_stmt acc stmts
  | Sif (_, s1, s2) -> collect_vars_stmt (collect_vars_stmt acc s1) s2
  | _ -> acc
;;

let compile_binop = function
  | Badd -> "+"
  | Bsub -> "-"
  | Bmul -> "*"
  | Bdiv -> "/"
  | Bmod -> "%"
  | Beq -> "=="
  | Bneq -> "!="
  | Blt -> "<"
  | Ble -> "<="
  | Bgt -> ">"
  | Bge -> ">="
  | _ -> failwith "unsupported operator"
;;

let compile_typ = function
  | Tint8 -> "int8_t"
  | Tint16 -> "int16_t"
  | Tint32 -> "int32_t"
  | Tint64 -> "int64_t"
  | Tuint8 -> "uint8_t"
  | Tuint16 -> "uint16_t"
  | Tuint32 -> "uint32_t"
  | Tuint64 -> "uint64_t"
  | _ -> failwith "unsupported type"
;;

let rec compile_expr buf = function
  | Ecst (Cint n) -> Buffer.add_string buf (string_of_int n)
  | Eident id -> Buffer.add_string buf id.id
  | Eunop (Uneg, e) ->
    Buffer.add_string buf "-(";
    compile_expr buf e;
    Buffer.add_char buf ')'
  | Ebinop (op, e1, e2) ->
    compile_expr buf e1;
    Buffer.add_char buf ' ';
    Buffer.add_string buf (compile_binop op);
    Buffer.add_char buf ' ';
    compile_expr buf e2
  | _ -> failwith "unsupported expression"
;;

let rec compile_stmt buf indent = function
  | Sdefine (id, typ, e) ->
    Buffer.add_string buf (String.make indent ' ');
    Buffer.add_string buf (compile_typ typ);
    Buffer.add_char buf ' ';
    Buffer.add_string buf id.id;
    Buffer.add_string buf " = ";
    compile_expr buf e;
    Buffer.add_string buf ";\n"
  | Sassign (id, e) ->
    Buffer.add_string buf (String.make indent ' ');
    Buffer.add_string buf id.id;
    Buffer.add_string buf " = ";
    compile_expr buf e;
    Buffer.add_string buf ";\n"
  | Sprint args ->
    Buffer.add_string buf (String.make indent ' ');
    let fmt = String.concat " " (List.map (fun _ -> "%d") args) in
    Buffer.add_string buf (Printf.sprintf "printf(\"%s\\n\"" fmt);
    List.iter
      (fun e ->
         Buffer.add_string buf ", ";
         compile_expr buf e)
      args;
    Buffer.add_string buf ");\n"
  | Sblock stmts ->
    Buffer.add_string buf (String.make indent ' ');
    Buffer.add_string buf "{\n";
    List.iter (compile_stmt buf (indent + 2)) stmts;
    Buffer.add_string buf (String.make indent ' ');
    Buffer.add_string buf "}\n"
  | Sif (cond, then_, else_) ->
    Buffer.add_string buf (String.make indent ' ');
    Buffer.add_string buf "if (";
    compile_expr buf cond;
    Buffer.add_string buf ")\n";
    compile_stmt buf indent then_;
    (match else_ with
     | Sblock [] -> ()
     | _ ->
       Buffer.add_string buf (String.make indent ' ');
       Buffer.add_string buf "else\n";
       compile_stmt buf indent else_)
  | _ -> failwith "unsupported statement"
;;

let compile (program : file) : string =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "#include <stdio.h>\n#include <stdint.h>\n\nint main(void)\n{\n";
  let vars = List.fold_left collect_vars_stmt [] program in
  List.iter
    (fun v -> Buffer.add_string buf (Printf.sprintf "  int %s = 0;\n" v))
    (List.rev vars);
  if vars <> [] then Buffer.add_char buf '\n';
  List.iter (compile_stmt buf 2) program;
  Buffer.add_string buf "  return 0;\n}\n";
  Buffer.contents buf
;;
