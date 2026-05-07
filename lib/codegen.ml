(* Code generation: AST -> C source *)

open Ast

let rec collect_vars_stmt acc = function
  | Sassign (id, _) -> if List.mem id.id acc then acc else id.id :: acc
  | Sblock stmts -> List.fold_left collect_vars_stmt acc stmts
  | Sif (_, s1, s2) -> collect_vars_stmt (collect_vars_stmt acc s1) s2
  | Smatch (_, cases) -> List.fold_left (fun a (_, s) -> collect_vars_stmt a s) acc cases
  | _ -> acc
;;

let compile_unop = function
  | Uneg -> "-"
  | Unot -> "!"
  | Usqrt -> "sqrt"
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
  | _ -> failwith "unsupported operator"
;;

let compile_typ = function
  | Tint8 -> "int8_t"
  | Tint16 -> "int16_t"
  | Tint32 -> "int32_t"
  | Tuint8 -> "uint8_t"
  | Tuint16 -> "uint16_t"
  | Tuint32 -> "uint32_t"
  | Tbool -> "int8_t"
  | _ -> failwith "unsupported type"
;;

let rec compile_expr buf expr =
  match expr.expr_node with
  | Ecst (Cint n) -> Buffer.add_string buf (string_of_int n)
  | Ecst (Cbool b) -> Buffer.add_string buf (if b then "1" else "0")
  | Eident id -> Buffer.add_string buf id.id
  | Eunop (op, e) ->
    Buffer.add_string buf (compile_unop op);
    Buffer.add_char buf '(';
    compile_expr buf e;
    Buffer.add_char buf ')'
  | Ebinop (Bpow, e1, e2) ->
    (*| Ebinop (Bpow, e1, e2) ->let vars = List.fold_left collect_vars_stmt [] program in
List.iter
  (fun v -> Buffer.add_string buf (Printf.sprintf "  int %s = 0;\n" v))
  (List.rev vars);*)
    Buffer.add_string buf "pow(";
    compile_expr buf e1;
    Buffer.add_string buf ", ";
    compile_expr buf e2;
    Buffer.add_string buf ")"
  | Ebinop (op, e1, e2) ->
    compile_expr buf e1;
    Buffer.add_char buf ' ';
    Buffer.add_string buf (compile_binop op);
    Buffer.add_char buf ' ';
    compile_expr buf e2
  | Ebufread (buf_expr, idx_expr) ->
    Buffer.add_string buf "(assert(";
    compile_expr buf idx_expr;
    Buffer.add_string buf " >= 0 && ";
    compile_expr buf idx_expr;
    Buffer.add_string buf " < ";
    compile_expr buf buf_expr;
    Buffer.add_string buf ".len), ";
    compile_expr buf buf_expr;
    Buffer.add_string buf ".data[";
    compile_expr buf idx_expr;
    Buffer.add_string buf "])"
  | Ebuflen buf_expr ->
    compile_expr buf buf_expr;
    Buffer.add_string buf ".len"
  | Ecall (id, args) ->
    Buffer.add_string buf id.id;
    Buffer.add_char buf '(';
    List.iteri
      (fun i arg ->
         if i > 0 then Buffer.add_string buf ", ";
         compile_expr buf arg)
      args;
    Buffer.add_string buf ")"
  | _ -> failwith "unsupported expression"

and handle_func id typ params body indent buf =
  Buffer.add_string buf (String.make indent ' ');
  Buffer.add_string buf (compile_typ typ);
  Buffer.add_char buf ' ';
  Buffer.add_string buf id.id;
  Buffer.add_char buf '(';
  let param_strs =
    List.map
      (fun (parameter_name, parameter_typ) ->
         compile_typ parameter_typ ^ " " ^ parameter_name.id)
      params
  in
  Buffer.add_string buf (String.concat ", " param_strs);
  Buffer.add_string buf ")";
  compile_stmt buf indent body

and compile_stmt buf indent = function
  | Sdefine (is_mut, id, typ, e) ->
    Buffer.add_string buf (String.make indent ' ');
    if not is_mut then Buffer.add_string buf "static ";
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
    Buffer.add_char buf '\n';
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
  | Smatch (e, cases) ->
    Buffer.add_string buf (String.make indent ' ');
    Buffer.add_string buf "switch (";
    compile_expr buf e;
    Buffer.add_string buf ") {\n";
    List.iter
      (fun (pat, s) ->
         Buffer.add_string buf (String.make indent ' ');
         (match pat with
          | Pwildcard -> Buffer.add_string buf "default"
          | Pconst (Cint n) ->
            Buffer.add_string buf "case ";
            Buffer.add_string buf (string_of_int n)
          | Pconst (Cbool b) ->
            Buffer.add_string buf "case ";
            Buffer.add_string buf (if b then "1" else "0")
          | _ -> failwith "unsupported pattern");
         Buffer.add_string buf ":\n";
         compile_stmt buf (indent + 2) s;
         Buffer.add_string buf (String.make (indent + 2) ' ');
         Buffer.add_string buf "break;\n")
      cases;
    Buffer.add_string buf (String.make indent ' ');
    Buffer.add_string buf "}\n"
  | Sbuffer (name, buf_ty, init_exprs) ->
    let elem_ty, size_expr =
      match buf_ty with
      | Tbuffer (e, s) -> e, s
      | _ -> failwith "expected buffer type"
    in
    let n =
      match size_expr.expr_node with
      | Ecst (Cint n) -> n
      | _ -> failwith "buffer size must be a constant"
    in
    let c_ty = compile_typ elem_ty in
    Buffer.add_string buf (String.make indent ' ');
    Buffer.add_string
      buf
      (Printf.sprintf
         "struct { %s data[%d]; int32_t len; int32_t cap; } %s = { {"
         c_ty
         n
         name.id);
    (match init_exprs with
     | [] -> Buffer.add_string buf "0"
     | _ ->
       List.iteri
         (fun i e ->
            if i > 0 then Buffer.add_string buf ", ";
            compile_expr buf e)
         init_exprs);
    Buffer.add_string buf (Printf.sprintf "}, %d, %d };\n" (List.length init_exprs) n)
  | Sbufwrite (buf_expr, val_expr) ->
    Buffer.add_string buf (String.make indent ' ');
    Buffer.add_string buf "assert(";
    compile_expr buf buf_expr;
    Buffer.add_string buf ".len < ";
    compile_expr buf buf_expr;
    Buffer.add_string buf ".cap);\n";
    Buffer.add_string buf (String.make indent ' ');
    compile_expr buf buf_expr;
    Buffer.add_string buf ".data[";
    compile_expr buf buf_expr;
    Buffer.add_string buf ".len++] = ";
    compile_expr buf val_expr;
    Buffer.add_string buf ";\n"
  | Sreturn e ->
    Buffer.add_string buf (String.make indent ' ');
    Buffer.add_string buf "return ";
    compile_expr buf e;
    Buffer.add_char buf ';'
  | Sfunc (id, typ, params, body) -> handle_func id typ params body indent buf
  | _ -> failwith "unsupported statement"
;;

let compile (program : file) : string =
  let buf = Buffer.create 256 in
  Buffer.add_string
    buf
    "#include <stdio.h>\n\
     #include <stdint.h>\n\
     #include <math.h>\n\
     #include <assert.h>\n\n\
     int main(void)\n\
     {\n";
  (*let vars = List.fold_left collect_vars_stmt [] program in
    List.iter
    (fun v -> Buffer.add_string buf (Printf.sprintf "  int %s = 0;\n" v))
    (List.rev vars);
  if vars <> [] then Buffer.add_char buf '\n';*)
  List.iter (compile_stmt buf 2) program;
  Buffer.add_string buf "  return 0;\n}\n";
  Buffer.contents buf
;;
