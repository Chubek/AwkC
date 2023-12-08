type cfg_node =
  | StatementNode of statement
  | EntryNode
  | ExitNode

and statement =
  | IfStatementNode of expr * cfg_node * cfg_node
  | WhileStatementNode of expr * cfg_node * cfg_node
  | ForStatementNode of statement option * expr option * statement option * cfg_node * cfg_node
  | ForInStatementNode of identifier * identifier * cfg_node
  | TerminatableStatementNode of terminatable_statement
  | EmptyStatementNode

and terminatable_statement =
  | SimpleStatementNode of simple_statement
  | BreakNode
  | ContinueNode
  | NextNode
  | ExitNode of expr option
  | ReturnNode of expr option
  | DoWhileStatementNode of cfg_node * expr

and simple_statement =
  | PrintAssignNode of lvalue * print_expr
  | AssignmentNode of lvalue * expr
  | DeleteNode of string * expr list
  | ExprStatementNode of expr
  | PrintStatementNode of print_expr list * output_redirection option
  | PrintfStatementNode of string * print_expr list * output_redirection option

and print_expr =
  | UnaryPrintExprNode of string * print_expr
  | NonUnaryPrintExprNode of non_unary_print_expr

and non_unary_print_expr =
  | ParenthesizedPrintExprNode of print_expr list
  | NonUnaryInputFunctionNode of non_unary_input_function

and non_unary_input_function =
  | SimpleGetNode
  | SimpleGetWithExprNode of expr
  | PipeInputFunctionNode of non_unary_expr * simple_get

and unary_input_function =
  | PipeInputFunctionNode of unary_expr * simple_get

and lvalue =
  | SimpleNameNode of identifier
  | IndexedNameNode of expr list
  | FieldRefNode of expr

and simple_get =
  | GetlineNode
  | GetlineWithLvalueNode of lvalue

and output_redirection =
  | OutputRedirectNode of string * expr
  | AppendRedirectNode of expr
  | PipeRedirectNode of expr

and expr =
  | VariableNode of string
  | ConstantNode of string
  | RegexNode of string
  | BinaryOpNode of expr * binary_op * expr
  | UnaryOpNode of unary_op * expr
  | PostfixOpNode of expr * unary_op
  | TernaryOpNode of expr * expr * expr
  | FunctionCallNode of identifier * expr list

type cfg_edge =
  | ControlFlowEdge of cfg_node * cfg_node
  | ConditionalEdge of expr * cfg_node * cfg_node

type control_flow_graph = {
  nodes: cfg_node list;
  edges: cfg_edge list;
}

