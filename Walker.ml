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
    walk_expr e fh;
    Printf.fprintf fh ")";
  | InList (el1, id) ->
    with ls_name = request_context in
    Printf.fprintf fh "void* %s[] = {" ls_name;
    List.map (fun e -> Printf.fprintf fh "(void*)";
                       walk_expr fh e; 
                       Printf.fprintf fh ",") el1;
    Printf.fprintf fh "NULL";
    Printf.fprintf fh "};"
    Printf.fprintf fh "for (void* item = &%s; item != NULL; item++) { callback_%s(); }" ls_name ls_name

and walk_lvalue lval =
  match lval with
  | SimpleName id -> walk_identifier id
  | ArrayAccess (id, indices) ->
    walk_identifier id;
    List.iter walk_expr indices
  | FieldRef e -> walk_expr e

and walk_simple_statement stmt =
  match stmt with
  | PrintAssign (lval, print_expr) ->
    walk_lvalue lval;
    walk_print_expr print_expr
  | Assignment (lval, e) ->
    walk_lvalue lval;
    walk_expr e
  (* Handle other cases similarly *)

and walk_print_expr print_expr =
  match print_expr with
  | UnaryPrintExpr (s, pe) ->
    Printf.printf "Unary Print Expression: %s\n" s;
    walk_print_expr pe
  | NonUnaryPrintExpr non_unary_pe ->
    walk_non_unary_print_expr non_unary_pe

and walk_non_unary_print_expr non_unary_pe =
  match non_unary_pe with
  | ParenthesizedPrintExpr expr_list ->
    List.iter walk_expr expr_list
  | GetLine getline ->
    walk_getline getline

and walk_getline getline =
  match getline with
  | SimpleGet -> Printf.printf "Simple Getline\n"
  | SimpleGetWithLval lval ->
    Printf.printf "Simple Getline with Lvalue\n";
    walk_lvalue lval
  (* Handle other cases similarly *)

and walk_output_redirection output_redirection =
  match output_redirection with
  | OutputRedirect (s, e) ->
    Printf.printf "Output Redirect: %s\n" s;
    walk_expr e
  (* Handle other cases similarly *)

and walk_statement stmt =
  match stmt with
  | IfStatement (cond, true_branch, false_branch_opt) ->
    walk_expr cond;
    walk_statement true_branch;
    (match false_branch_opt with
    | Some false_branch -> walk_statement false_branch
    | None -> ())
  | WhileStatement (cond, body) ->
    walk_expr cond;
    walk_statement body
  (* Handle other cases similarly *)

and walk_terminatable_statement term_stmt =
  match term_stmt with
  | SimpleStatement simple_stmt -> walk_simple_statement simple_stmt
  | Break -> Printf.printf "Break\n"
  (* Handle other cases similarly *)

and walk_pattern pattern =
  match pattern with
  | NormalPattern (e, None) ->
    walk_expr e
  | NormalPattern (e, Some e_opt) ->
    walk_expr e;
    walk_expr e_opt
  | SpecialPattern sp ->
    Printf.printf "Special Pattern: %s\n" (match sp with (* ... *))

and walk_action action =
  match action with
  | EmptyAction -> Printf.printf "Empty Action\n"
  | BlockAction stmt_list ->
    Printf.printf "Block Action\n";
    List.iter walk_statement stmt_list
  (* Handle other cases similarly *)

and walk_item item =
  match item with
  | SoloAction act -> walk_action act
  | PatternAction (pat, act) ->
    walk_pattern pat;
    walk_action act
  (* Handle other cases similarly *)

and walk_program program =
  match program with
  | Program items ->
    Printf.printf "AWK Program\n";
    List.iter walk_item items

(* Example usage *)
let example_ast =
  Program [
    SoloAction (BlockAction [
      SimpleStatement (Assignment (SimpleName "x", Number 42));
      IfStatement (BinaryOp (Variable "x", Eq, Number 42),
                    SimpleStatement (PrintAssign (SimpleName "y", UnaryPrintExpr ("-", Number 42))),
                    SimpleStatement (PrintStatement ([NonUnaryPrintExpr (ParenthesizedPrintExpr [Variable "x"])], None)));
    ]);
    (* ... add ore items as needed *)
  ]m
