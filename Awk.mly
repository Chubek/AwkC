%token <string> NAME
%token <float> NUMBER
%token <string> STRING
%token <string> ERE

%token BEGIN END BREAK CONTINUE DELETE DO ELSE EXIT FOR FUNCTION IF IN
%token NEXT PRINT PRINTF RETURN WHILE

%token BUILTIN_FUNC_NAME
%token GETLINE

%token "+=" "-=" "*=" "/=" "%=" "^="
%token "||" "&&" "!~" "==" "<=" ">=" "!=" "++" "--" ">>"

%token '{' '}' '(' ')' '[' ']' ',' ';' NEWLINE
%token '+' '-' '*' '%' '^' '!' '>' '<' '|' '?' ':' '˜' '$' '='

%type <expr>   expr, unary_expr, non_unary_expr
%type <lvalue> lvalue
%type <special_pattern> special_pattern
%type <(expr * expr option)> normal_pattern
%type <statement> terminated_statement, unterminated_statement
%type <simple_statement> simple_statement

%start <AST.program> program
%%

program          : item_list               { $1 }
                 | item_list item          { $1 @ [$2] }

item_list        : /* empty */             { [] }
                 | item_list item          { $1 @ [$2] }

item             : action                  { Action($1) }
                 | pattern action          { PatternAction($1, $2) }
                 | normal_pattern          { NormalPattern($1) }
                 | FUNCTION NAME '(' param_list_opt ')' newline_opt action
                                           { Function($2, $4, $6) }
                 | FUNCTION FUNC_NAME '(' param_list_opt ')' newline_opt action
                                           { Function($2, $4, $6) }

param_list_opt   : /* empty */             { [] }
                 | param_list              { $1 }

param_list       : NAME                    { [$1] }
                 | param_list ',' NAME     { $1 @ [$3] }

pattern          : normal_pattern           { NormalPattern($1) }
                 | special_pattern          { SpecialPattern($1) }

normal_pattern   : expr                      { ($1. None) }
                 | expr ',' newline_opt expr { ($1, Some ($4)) }

special_pattern  : BEGIN                    { Begin }
                 | END                      { End }

action           : '{' newline_opt '}'     { [] }
                 | '{' newline_opt terminated_statement_list '}'
                                           { $3 }
                 | '{' newline_opt unterminated_statement_list '}'
                                           { $3 }

terminator       : terminator NEWLINE       { $1 }
                 | ';'                      { [] }
                 | NEWLINE                  { [] }

terminated_statement_list : terminated_statement
                                           { [$1] }
                 | terminated_statement_list terminated_statement
                                           { $1 @ [$2] }

unterminated_statement_list : unterminated_statement
                                           { [$1] }
                 | terminated_statement_list unterminated_statement
                                           { $1 @ [$2] }

terminated_statement : action newline_opt    { $1 }
                 | IF '(' expr ')' newline_opt terminated_statement
                                           { IfStatement($3, $5, None) }
                 | IF '(' expr ')' newline_opt terminated_statement
                       ELSE newline_opt terminated_statement
                                           { IfStatement($3, $5, Some($8)) }
                 | WHILE '(' expr ')' newline_opt terminated_statement
                                           { WhileStatement($3, $5) }
                 | FOR '(' simple_statement_opt ';' expr_opt ';'
                      simple_statement_opt ')' newline_opt terminated_statement
                                           { ForStatement(Some $3, Some $5, Some $7, $10) }
                 | FOR '(' NAME IN NAME ')' newline_opt terminated_statement
                                           { ForInStatement($4, $6, $8) }
                 | ';' newline_opt          { [] }
                 | terminatable_statement NEWLINE newline_opt
                                           { [$1] }
                 | terminatable_statement ';' newline_opt
                                           { [$1] }

unterminated_statement : terminatable_statement
                                           { [$1] }
                 | IF '(' expr ')' newline_opt unterminated_statement
                                           { IfStatement($3, $5, None) }
                 | IF '(' expr ')' newlineopt terminated_statement
                       ELSE newline_opt unterminated_statement
                                           { IfStatement($3, $5, Some ($8)) }
                 | WHILE '(' expr ')' newline_opt unterminated_statement
                                           { WhileStatement($3, $5) }
                 | FOR '(' simple_statement_opt ';' expr_opt ';'
                      simple_statement_opt ')' newline_opt unterminated_statement
                                           { ForStatement(Some $3, Some $5, Some $7, $10) }
                 | FOR '(' NAME IN NAME ')' newline_opt unterminated_statement
                                           { ForInStatement($4, $6, $9) }

terminatable_statement : simple_statement
                                           { $1 }
                 | BREAK                    { Break }
                 | CONTINUE                 { Continue }
                 | NEXT                    { Next }
                 | EXIT expr_opt            { Exit($2) }
                 | RETURN expr_opt          { Return($2) }
                 | DO newline_opt terminated_statement WHILE '(' expr ')'
                                           { DoWhile($5, $9) }

simple_statement_opt : /* empty */           { [] }
                 | simple_statement         { [$1] }

simple_statement : DELETE NAME '[' expr_list ']'
                                           { Delete($2, $4) }
                 | expr                     { ExprStatement($1) }
                 | print_statement          { $1 }

print_statement  : simple_print_statement   { $1 }
                 | simple_print_statement output_redirection
                                           { $1 @ [$2] }

simple_print_statement : PRINT  print_expr_list_opt
                                           { Print($2) }
                 | PRINT  '(' multiple_expr_list ')'
                                           { Print($3) }
                 | PRINTF print_expr_list   { Printf($2) }
                 | PRINTF '(' multiple_expr_list ')'
                                           { Printf($3) }

output_redirection : '>' expr                { OutputRedirect($2, OutputRedirectAppend(false)) }
                 | ">>" expr                { OutputRedirect($2, OutputRedirectAppend(true)) }
                 | '|' expr                 { OutputRedirect($2, OutputRedirectPipe) }

expr_list_opt    : /* empty */              { [] }
                 | expr_list                { $1 }

expr_list        : expr                     { [$1] }
                 | multiple_expr_list       { $1 }

multiple_expr_list : expr ',' newline_opt expr
                                           { [$1; $3] }
                 | multiple_expr_list ',' newline_opt expr
                                           { $1 @ [$3] }

expr_opt         : /* empty */              { None }
                 | expr                     { Some $1 }

expr             : unary_expr               { $1 }
                 | non_unary_expr           { $1 }

unary_expr       : '+' expr                 { UnaryOp(Pos, $2) }
                 | '-' expr                 { UnaryOp(Neg, $2) }
                 | unary_expr '^' expr       { BinaryOp(Exp, $1, $3) }
                 | unary_expr '*' expr       { BinaryOp(Mul, $1, $3) }
                 | unary_expr '/' expr       { BinaryOp(Div, $1, $3) }
                 | unary_expr '%' expr       { BinaryOp(Rem, $1, $3) }
                 | unary_expr '+' expr       { BinaryOp(Add, $1, $3) }
                 | unary_expr '-' expr       { BinaryOp(Sub, $1, $3) }
                 | unary_expr non_unary_expr { BinaryOp(NoOp, $1, $3) }
                 | unary_expr '<' expr       { BinaryOp(Lt, $1, $3) }
                 | unary_expr "<=" expr      { BinaryOp(Le, $1, $3) }
                 | unary_expr "!=" expr      { BinaryOp(Ne, $1, $3) }
                 | unary_expr "==" expr      { BinaryOp(Eq, $1, $3) }
                 | unary_expr '>' expr       { BinaryOp(Gt, $1, $3) }
                 | unary_expr ">=" expr      { BinaryOp(Ge, $1, $3) }
                 | unary_expr '˜' expr       { BinaryOp(Ere, $1, $3) }
                 | unary_expr "!~" expr      { BinaryOp(Nre, $1, $3) }
                 | unary_expr IN NAME        { UnaryOp(In, $1) }
                 | unary_expr "&&" newline_opt expr
                                           { BinaryOp(And, $1, $3) }
                 | unary_expr "||" newline_opt expr
                                           { BinaryOp(Or, $1, $3) }
                 | unary_expr '?' expr ':' expr
                                           { TernaryOp($1, $3, $5) }
                 | unary_input_function     { $1 }

non_unary_expr   : '(' expr ')'              { $2 }
                 | '!' expr                  { UnaryOp(Not, $2) }
                 | non_unary_expr '^' expr   { BinaryOp(Exp, $1, $3) }
                 | non_unary_expr '*' expr   { BinaryOp(Mul, $1, $3) }
                 | non_unary_expr '/' expr   { BinaryOp(Div, $1, $3) }
                 | non_unary_expr '%' expr   { BinaryOp(Rem, $1, $3) }
                 | non_unary_expr '+' expr   { BinaryOp(Add, $1, $3) }
                 | non_unary_expr '-' expr   { BinaryOp(Sub, $1, $3) }
                 | non_unary_expr non_unary_expr
                                           { BinaryOp(NoOp, $1, $2) }
                 | non_unary_expr '<' expr   { BinaryOp(Lt, $1, $3) }
                 | non_unary_expr "<=" expr  { BinaryOp(Le, $1, $3) }
                 | non_unary_expr "!=" expr  { BinaryOp(Ne, $1, $3) }
                 | non_unary_expr "==" expr  { BinaryOp(Eq, $1, $3) }
                 | non_unary_expr '>' expr   { BinaryOp(Gt, $1, $3) }
                 | non_unary_expr ">=" expr  { BinaryOp(Ge, $1, $3) }
                 | non_unary_expr '˜' expr   { BinaryOp(Ere, $1, $3) }
                 | non_unary_expr "!~" expr  { BinaryOp(Nre, $1, $3) }
                 | non_unary_expr IN NAME    { In($1, $3) }
                 | '(' multiple_expr_list ')' IN NAME
                                           { InList($2, $5) }
                 | non_unary_expr "&&" newline_opt expr
                                           { BinaryOp(And, $1, $3) }
                 | non_unary_expr "||" newline_opt expr
                                           { BinaryOp(Or, $1, $3) }
                 | non_unary_expr '?' expr ':' expr
                                           { TernaryOp($1, $3, $5) }
                 | NUMBER                   { Number($1) }
                 | STRING                   { String($1) }
                 | lvalue                   { Lvalue($1) }
                 | ERE                      { Regex($1) }
                 | lvalue "++"              { PostfixOp($1, Incr) }
                 | lvalue "--"              { PostfixOp($1, Decr) }
                 | "++" lvalue              { UnaryOp(Incr, $2) }
                 | "--" lvalue              { UnaryOp(Decr, $2) }
                 | lvalue "^=" expr         { BinaryOp(ExpAssign, $1, $3) }
                 | lvalue "%=" expr         { BinaryOp(RemAssign, $1, $3) }
                 | lvalue "*=" expr         { BinaryOp(MulAssign, $1, $3) }
                 | lvalue "/=" expr         { BinaryOp(DivAssign, $1, $3) }
                 | lvalue "+=" expr         { BinaryOp(AddAssign, $1, $3) }
                 | lvalue "-=" expr         { BinaryOp(SubAssign, $1, $3) }
                 | lvalue '=' expr          { Assignment($1, $3) }
                 | FUNC_NAME '(' expr_list_opt ')'
                                           { FunctionCall($1, $3) }
                 | BUILTIN_FUNC_NAME '(' expr_list_opt ')'
                                           { BuiltinFunctionCall($1, $3) }
                 | non_unary_input_function { $1 }
                 ;

lvalue           :
                 | NAME { SimpleName $1 }
                 | NAME '[' expr_list ']' { ArrayAccess($1, $3) }
                 | '$' expr { FieldRef $2 }
                 ;

non_unary_input_function:
                 | simple_get { $1 }
                 | simple_get '<' expr { SimpleGetWithExpr($1, $3) }
                 | non_unary_expr '|' simple_get { SimpleGetWithPipe($1, $3) }
                 ;

unary_input_function:
                 | unary_expr '|' simple_get { SimpleGetWithPipe($1, $3) }
                 ;

simple_get       :
                 | GETLINE { SimpleGet }
                 | GETLINE lvalue { SimpleGetWithLval($2) }
                 ;

newline_opt      :
                 | /* empty */ { [] }
                 | newline_opt NEWLINE { $1 }
                 ;

_
