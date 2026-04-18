(* Code generation: AST -> C source *)

open Ast

(* Collect all variable names assigned anywhere in a statement tree.
   Used to emit declarations at the top of main(). *)
let rec collect_vars_stmt acc = function
  | Sassign (id, _) -> if List.mem id.id acc then acc else id.id :: acc
  | Sblock stmts -> List.fold_left collect_vars_stmt acc stmts
  | Sif (_, s1, s2) ->
    let acc = collect_vars_stmt acc s1 in
    collect_vars_stmt acc s2
  | Swhile (_, body) -> collect_vars_stmt acc body
  | Sprint _ -> acc
;;

let compile_unop = function
  | Uneg -> "-"
  | Unot -> "!"
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
  | Band -> "&&"
  | Bor -> "||"
;;

let rec compile_expr buf = function
  | Ecst (Cint n) -> Buffer.add_string buf (string_of_int n)
  | Ecst (Cbool b) -> Buffer.add_string buf (if b then "1" else "0")
  | Eident id -> Buffer.add_string buf id.id
  | Eunop (op, e) ->
    Buffer.add_string buf (compile_unop op);
    Buffer.add_char buf '(';
    compile_expr buf e;
    Buffer.add_char buf ')'
  | Ebinop (op, e1, e2) ->
    Buffer.add_char buf '(';
    compile_expr buf e1;
    Buffer.add_char buf ' ';
    Buffer.add_string buf (compile_binop op);
    Buffer.add_char buf ' ';
    compile_expr buf e2;
    Buffer.add_char buf ')'
;;

(* Build a comma-separated printf format string and argument list for Sprint. *)
let compile_print buf args =
  let fmt = String.concat " " (List.map (fun _ -> "%d") args) in
  Buffer.add_string buf (Printf.sprintf "  printf(\"%s\\n\"" fmt);
  List.iter
    (fun e ->
       Buffer.add_string buf ", ";
       compile_expr buf e)
    args;
  Buffer.add_string buf ");\n"
;;

let rec compile_stmt buf indent = function
  | Sassign (id, e) ->
    Buffer.add_string buf (String.make indent ' ');
    Buffer.add_string buf id.id;
    Buffer.add_string buf " = ";
    compile_expr buf e;
    Buffer.add_string buf ";\n"
  | Sprint args ->
    Buffer.add_string buf (String.make indent ' ');
    compile_print buf args
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
  | Swhile (cond, body) ->
    Buffer.add_string buf (String.make indent ' ');
    Buffer.add_string buf "while (";
    compile_expr buf cond;
    Buffer.add_string buf ")\n";
    compile_stmt buf indent body
;;

let compile (program : file) : string =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "#include <stdio.h>\n\nint main(void)\n{\n";
  (* Declare all variables at the top of main *)
  let vars = collect_vars_stmt [] program in
  List.iter
    (fun v -> Buffer.add_string buf (Printf.sprintf "  int %s = 0;\n" v))
    (List.rev vars);
  if vars <> [] then Buffer.add_char buf '\n';
  (* Emit the program body (top-level is always Sblock) *)
  (match program with
   | Sblock stmts -> List.iter (compile_stmt buf 2) stmts
   | single -> compile_stmt buf 2 single);
  Buffer.add_string buf "  return 0;\n}\n";
  Buffer.contents buf
;;
