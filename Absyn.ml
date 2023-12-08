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
  | Incr | Decr | Neg | Pos | BitwiseNot | Not

and expr =
  | Variable of string
  | Constant of string
  | Regex of string
  | BinaryOp of expr * binary_op * expr
  | UnaryOp of unary_op * expr
  | PostfixOp of expr * unary_op
  | TernaryOp of expr * expr * expr
  | FunctionCall of identifier * expr list

and built_in_function =
  | Atan2 of expr * expr
  | Cos of expr
  | Sin of expr
  | Exp of expr
  | Log of expr
  | Sqrt of expr
  | Int of expr
  | Rand
  | Srand of expr
  | Gsub of expr * expr * expr
  | Index of expr * expr
  | Length of expr
  | Match of expr * expr
  | Split of expr * expr * expr_array
  | Sprintf of expr * expr_array
  | Sub of expr * expr * expr
  | Substr of expr * expr * expr option
  | Tolower of expr
  | Toupper of expr
  | Close of expr
  | System of expr

and item =
  | SoloAction of action
  | PatternAction of pattern * action
  | NameFunction of string * param list option * action
  | IdentFunction of identifier * param list option * action

and special_pattern =
  | Begin | End | BeginFile | EndFile

and pattern =
  | NormalPattern of expr
  | SpecialPattern of special_pattern

and action =
  | EmptyAction
  | BlockAction of statement list

and statement =
  | IfStatement of expr * statement * statement option
  | WhileStatement of expr * statement
  | ForStatement of statement option * expr option * statement option * statement
  | ForInStatement of identifier in identifier
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
  | NonUnaryInputFunction of non_unary_input_function

and non_unary_input_function =
  | SimpleGet
  | SimpleGetWithExpr of expr
  | PipeInputFunction of non_unary_expr * simple_get

and unary_input_function =
  | PipeInputFunction of unary_expr * simple_get

and lvalue =
  | SimpleName of identifier
  | IndexedName of expr list
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

