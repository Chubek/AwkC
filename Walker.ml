let rec walk_expr expr fh =
  match expr with
  | Variable id -> Printf.fprintf fh "%s" id
  | Constant e -> walk_expr e fh
  | Number n -> Printf.fprintf fh "%d" n
  | String s -> Printf.fprintf fh "\"%s\"" s
  | Regex r -> Printf.fprintf h "regex_match(\"%s\")" r
  | Lvalue lval -> walk_lvalue lval fh
  | BinaryOp (e1, op, e2) ->
    walk_expr e1 fh;
    Printf.fprintf fh "(";
    walk_binary_op op fh;
    Printf.fprintf fh ")";
    walk_expr e2 fh
  | UnaryOp (op, e) ->
    walk_unary_op op fh;
    Printf.fprintf fh "(";
    walk_expr e fh;
    Printf.fprintf fh ")"
  | PostfixOp (op, e) -> 
    walk_unary_op op fh;
    Printf.fprintf fh "(";
    walk_expr e fh;
    Printf.fprintf fh ")"
  | TernaryOp (e, e, e) ->
    Printf.fprintf fh "(";
    walk_expr e fh;
    Printf.fprintf fh ")";
    Printf.fprintf fh " ? ";
    Printf.fprintf fh "(";
    walk_expr e fh;
    Printf.fprintf fh ")";
    Printf.fprintf fh " : ";
    Printf.fprintf fh "(";
    walk_expr e fh
  | FunctionCall (id, arguments) ->
    Printf.fprintf "%s(" id;
    List.iter (fun exp -> walk_expr exp fh) arguments;
    Printf.fprintf ")"

and walk_lvalue lval fh =
  match lval with
  | SimpleName id -> Printf.fpintf fh "%s" id
  | ArrayAccess (id, indices) -> 
    Printf.fprintf  fh "%s[" id;
    List.iter (fun exp -> walk_expr exp fh) indices;
    Printf.fprintf fh "]"
  | FieldRef e ->
    Printf.fprintf "get_field("
    walk_expr e
    Printf.fprintf ")"

and walk_simple_statement stmt fh =
  match stmt with
  | PrintAssign (lval, print_expr) ->
    Printf.fprintf fh "unsigned char* ";
    walk_lvalue lval fh;
    Printf.fprintf fh " = ";
    walk_print_expr print_expr fh
  | Assignment (lval, e) ->
    Printf.fprintf fh "void* ";
    walk_lvalue lval fh;
    Printf.fprintf fh " = ";
    walk_expr e fh

and walk_print_expr print_expr =
  match print_expr with
  | UnaryPrintExpr (s, pe) ->
    Printf.fprintf fh "printf(\"%s\", " s;
    walk_print_expr fh pe;
    Printf.fprintf fh ")"
  | NonUnaryPrintExpr non_unary_pe ->
    walk_non_unary_print_expr non_unary_pe fh

and walk_non_unary_print_expr non_unary_pe fh =
  match non_unary_pe with
  | ParenthesizedPrintExpr expr_list ->
    List.iter walk_expr expr_list fh
  | GetLine getline ->
    walk_getline getline fh

and walk_getline getline fh =
  match getline with
  | SimpleGet -> Printf.fprint fh "getline()"
  | SimpleGetWithLval lval ->
    walk_lvalue lval fh;
    Printf.fprintf " = getline_retr()";
  | SimpleGetWithPipe (e, sg) ->
    Printf.fprintf fh "pipe_command_into(";
    walk_expr e fh;
    Printf.fprintf fh ", &(";
    walk_getline sg fh;
    Printf.fprintf fh ")"

and walk_output_redirection output_redirection fh =
  match output_redirection with
  | OutputRedirect (s, e) ->
    Printf.fprintf fh "redirect_output(\"%s\", (" s;
    walk_expr e fh;
    Printf.fprintf fh "))"
  | AppendRedirect (s, e) ->
    Printf.fprintf fh "append_output(\"%s\", (" s;
    walk_expr s e;
    Printf.fprintf fh "))"
  | PipeRedirect (s, e) ->
    Printf.fprintf fh "pipe_redirect(\"%s\", (" s;
    walk_expr s e;
    Printf.fprintf fh "))"

and walk_statement stmt fh =
  match stmt with
  | IfStatement (cond, true_branch, false_branch_opt) ->
    Printf.fprintf fh "if (";
    walk_expr cond fh;
    Printf.fprintf fh ") {";
    walk_statement true_branch fh;
    Printf.fprintf fh "}";
    (match false_branch_opt with
    | Some false_branch -> 
      Printf.fprintf fh "else {";
      walk_statement false_branch fh;
      Printf.fprintf fh "}"
    | None -> ())
  | WhileStatement (cond, body) ->
    Printf.fprintf fh "while (";
    walk_expr cond fh;
    Printf.fprintf fh ") {";
    walk_statement body fh;
    Printf.fprintf fh "}"
  | ForInStatement (i1, i2, s) ->
    Printf.fprintf fh "for (void* %s = %s; %s != NULL; %s++) {" i1 i2 i1 i1;
    walk_statement s fh;
    Printf.fprintf fh "}"
  | TerminatableStatement ts -> walk_terminatable_statement ts fh

and walk_terminatable_statement term_stmt fh =
  match term_stmt with
  | SimpleStatement simple_stmt -> 
    walk_simple_statement simple_stmt fh
  | Break -> Printf.fprintf fh "break"
  | Continue -> Printf.fprintf fh "continue"
  | Next -> Printf.fprintf fh "next()"
  | Exit opt_expr ->
       Printf.fprintf fh "exit(";
        match opt_expr with
       | Some e -> walk_expr e fh
       | None -> Printf.fprintf fh "0";
       Printf.fprintf fh ")"
  | Return of opt_expr ->
       Printf.fprintf fh "return(";
        match opt_expr with
       | Some e -> walk_expr e fh
       | None -> Printf.fprintf fh "0";
       Printf.fprintf fh ")"
  | DoWhileStatement (s, e) ->
        Printf.fprintf fh "do { ";
        walk_statement s fh;
        Printf.fprintf fh "while (";
        walk_expr e fh;
        Printf.fprintf fh ")"


and walk_pattern pattern fh =
  match pattern with
  | NormalPattern (e, e_opt) ->
        Printf.fprintf fh "if ("
        walk_expr e fh;
        match e_opt with
       | Some e -> 
          Printf.fprintf "&&";
          walk_expr e fh;
          Printf.fprintf  fh ")";
       | None -> Printf.fprintf fh ")";
  | SpecialPattern sp -> ()

and walk_action action fh =
  match action with
  | EmptyAction -> ()
  | BlockAction stmt_list ->
    List.iter (fun s -> walk_statement s and Printf.fprintf fh ";") stmt_list

and walk_item item fh =
  match item with
  | SoloAction act -> walk_action act fh
  | PatternAction (pat, act) ->
    walk_pattern pat fh;
    walk_action act fh

and walk_program program =
  match program with
  | Program items ->
    List.iter walk_item items
