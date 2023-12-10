type identifier = string
type param = string

type binary_op =
  | Exp | Mul | Div | Rem | Add | Sub
  | Ge  | Gt  | Le  | Lt  | Eq  | Ne
  | Ere | Nre | In  | And | Or  | AddAssign
  | SubAssign | MulAssign | RemAssing
  | DivAssign | ExpAssign | BitwiseOr
  | BitwiseXor| BitwiseAnd

and unary_op =
  | Incr | Decr | Neg | Pos | BitwiseNot | Not | In

and expr =
  | Variable of string
  | Constant of expr
  | Number of int
  | String of string
  | Regex of string
  | Lvalue of lvalue
  | BinaryOp of expr * binary_op * expr
  | UnaryOp of unary_op * expr
  | PostfixOp of expr * unary_op
  | TernaryOp of expr * expr * expr
  | InList of expr list * identifier
  | FunctionCall of identifier * expr list
  | BuiltinFunctionCall of identifier * expr list

and item =
  | SoloAction of action
  | PatternAction of pattern * action
  | NameFunction of string * param list option * action
  | IdentFunction of identifier * param list option * action

and special_pattern =
  | Begin | End

and pattern =
  | NormalPattern of (expr * expr option)
  | SpecialPattern of special_pattern

and action =
  | EmptyAction
  | BlockAction of statement list

and statement =
  | IfStatement of expr * statement * statement option
  | WhileStatement of expr * statement
  | ForStatement of statement option * expr option * statement option * statement
  | ForInStatement of identifier in identifier * statement
  | TerminatableStatement of terminatable_statement

and terminatable_statement =
  | SimpleStatement of simple_statement
  | Break
  | Continue
  | Next
  | Exit of expr option
  | Return of expr option
  | DoWhileStatement of statement * expr

and simple_statement =
  | PrintAssign of lvalue * print_expr
  | Assignment of lvalue * expr
  | Delete of string * expr list
  | ExprStatement of expr
  | PrintStatement of print_expr list * output_redirection option
  | PrintfStatement of string * print_expr list *  output_redirection option

and print_expr =
  | UnaryPrintExpr of string * print_expr
  | NonUnaryPrintExpr of non_unary_print_expr

and non_unary_print_expr =
  | ParenthesizedPrintExpr of print_expr list
  | GetLine of getline

and getline =
  | SimpleGet
  | SimpleGetWithLval of lval
  | SimpleGetWithExpr of getline * expr
  | SimpleGetWithPipe of expr * simple_get

and lvalue =
  | SimpleName of identifier
  | ArrayAccess of identifier * expr list
  | FieldRef of expr

and simple_get =
  | Getline
  | GetlineWithLvalue of lvalue

and output_redirection =
  | OutputRedirect of string * expr
  | AppendRedirect of expr
  | PipeRedirect of expr

and awk_program =
  | Program of item list

