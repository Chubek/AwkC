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

%start program
%%

program          : item_list
                 | item_list item
                 ;

item_list        : /* empty */
                 | item_list item terminator
                 ;

item             : action
                 | pattern action
                 | normal_pattern
                 | FUNCTION NAME '(' param_list_opt ')' newline_opt action
                 | FUNCTION FUNC_NAME '(' param_list_opt ')' newline_opt action
                 ;

param_list_opt   : /* empty */
                 | param_list
                 ;

param_list       : NAME
                 | param_list ',' NAME
                 ;

pattern          : normal_pattern
                 | special_pattern
                 ;

normal_pattern   : expr
                 | expr ',' newline_opt expr
                 ;

special_pattern  : BEGIN
                 | END
                 ;

action           : '{' newline_opt '}'
                 | '{' newline_opt terminated_statement_list '}'
                 | '{' newline_opt unterminated_statement_list '}'
                 ;

terminator       : terminator NEWLINE
                 | ';'
                 | NEWLINE
                 ;

terminated_statement_list : terminated_statement
                 | terminated_statement_list terminated_statement
                 ;

unterminated_statement_list : unterminated_statement
                 | terminated_statement_list unterminated_statement
                 ;

terminated_statement : action newline_opt
                 | IF '(' expr ')' newline_opt terminated_statement
                 | IF '(' expr ')' newline_opt terminated_statement
                       ELSE newline_opt terminated_statement
                 | WHILE '(' expr ')' newline_opt terminated_statement
                 | FOR '(' simple_statement_opt ';' expr_opt ';' simple_statement_opt ')' newline_opt terminated_statement
                 | FOR '(' NAME IN NAME ')' newline_opt terminated_statement
                 | ';' newline_opt
                 | terminatable_statement NEWLINE newline_opt
                 | terminatable_statement ';' newline_opt
                 ;

unterminated_statement : terminatable_statement
                 | IF '(' expr ')' newline_opt unterminated_statement
                 | IF '(' expr ')' newline_opt terminated_statement
                 
                       ELSE newline_opt unterminated_statement
                 | WHILE '(' expr ')' newline_opt unterminated_statement
                 | FOR '(' simple_statement_opt ';' expr_opt ';' simple_statement_opt ')' newline_opt unterminated_statement
                 | FOR '(' NAME IN NAME ')' newline_opt unterminated_statement
                 ;

terminatable_statement : simple_statement
                 | BREAK
                 | CONTINUE
                 | NEXT
                 | EXIT expr_opt
                 | RETURN expr_opt

                 | DO newline_opt terminated_statement WHILE '(' expr ')'
                 ;

simple_statement_opt : /* empty */
                 | simple_statement
                 ;

simple_statement : DELETE NAME '[' expr_list ']'
                 | expr
                 | print_statement
                 ;

print_statement  : simple_print_statement
                 | simple_print_statement output_redirection
                 ;

simple_print_statement : PRINT  print_expr_list_opt
                 | PRINT  '(' multiple_expr_list ')'
                 | PRINTF print_expr_list
                 | PRINTF '(' multiple_exr_list ')'
                 ;

output_redirection : '>' expr
                 | ">>" expr
                 | '|' expr
                 ;

expr_list_opt    : /* empty */
                 | expr_list
                 ;

expr_list        : expr
                 | multiple_expr_list
                 ;

multiple_expr_list : expr ',' newline_opt expr
                 | multiple_expr_list ',' newline_opt expr
                 ;

expr_opt         : /* empty */
                 | expr
                 ;

expr             : unary_expr
                 | non_unary_expr
                 ;

unary_expr       : '+' expr
                 | '-' expr
                 | unary_expr '^' expr
                 | unary_expr '*' expr
                 | unary_expr '/' expr
                 | unary_expr '%' expr
                 | unary_expr '+' expr
                 | unary_expr '-' expr
                 | unary_expr non_unary_expr
                 | unary_expr '<' expr
                 | unary_expr "<=" expr
                 | unary_expr "!=" expr
                 | unary_expr "==" expr
                 | unary_expr '>' expr
                 | unary_expr ">=" expr
                 | unary_expr '˜' expr
                 | unary_expr "!~" expr
                 | unary_expr IN NAME
                 | unary_expr "&&" newline_opt expr
                 | unary_expr "||" newline_opt expr
                 | unary_expr '?' expr ':' expr
                 | unary_input_function
                 ;

non_unary_expr   : '(' expr ')'
                 | '!' expr
                 | non_unary_expr '^' expr
                 | non_unary_expr '*' expr
                 | non_unary_expr '/' expr
                 | non_unary_expr '%' expr
                 | non_unary_expr '+' expr
                 | non_unary_expr '-' expr
                 | non_unary_expr non_unary_expr
                 | non_unary_expr '<' expr
                 | non_unary_expr "<=" expr
                 | non_unary_expr "!=" expr
                 | non_unary_expr "==" expr
                 | non_unary_expr '>' expr
                 | non_unary_expr ">=" expr
                 | non_unary_expr '˜' expr
                 | non_unary_expr "!~" expr
                 | non_unary_expr IN NAME
                 | '(' multiple_expr_list ')' IN NAME
                 | non_unary_expr "&&" newline_opt expr
                 | non_unary_expr "||" newline_opt expr
                 | non_unary_expr '?' expr ':' expr
                 | NUMBER
                 | STRING
                 | lvalue
                 | ERE
                 | lvalue "++"
                 | lvalue "--"
                 | "++" lvalue
                 | "--" lvalue
                 | lvalue "^=" expr
                 | lvalue "%=" expr
                 | lvalue "*=" expr
                 | lvalue "/=" expr
                 | lvalue "+=" expr
                 | lvalue "-=" expr
                 | lvalue '=' expr
                 | FUNC_NAME '(' expr_list_opt ')'
                 | BUILTIN_FUNC_NAME '(' expr_list_opt ')'
                 | BUILTIN_FUNC_NAME
                 | non_unary_input_function
                 ;

print_expr_list_opt : /* empty */
                 | print_expr_list
                 ;

print_expr_list  : print_expr
                 | print_expr_list ',' newline_opt print_expr
                 ;

print_expr       : unary_print_expr
                 | non_unary_print_expr
                 ;

unary_print_expr : '+' print_expr
                 | '-' print_expr
                 | unary_print_expr '^' print_expr
                 | unary_print_expr '*' print_expr
                 | unary_print_expr '/' print_expr
                 | unary_print_expr '%' print_expr
                 | unary_print_expr '+' print_expr
                 | unary_print_expr '-' print_expr
                 | unary_print_expr non_unary_print_expr
                 | unary_print_expr '˜' print_expr
                 | unary_print_expr "!~" print_expr
                 | unary_print_expr IN NAME
                 | unary_print_expr "&&" newline_opt print_expr
                 | unary_print_expr "||" newline_opt print_expr
                 | unary_print_expr '?' print_expr ':' print_expr
                 ;

non_unary_print_expr : '(' expr ')'
                 | '!' print_expr
                 | non_unary_print_expr '^' print_expr
                 | non_unary_print_expr '*' print_expr
                 | non_unary_print_expr '/' print_expr
                 | non_unary_print_expr '%' print_expr
                 | non_unary_print_expr '+' print_expr
                 | non_unary_print_expr '-' print_expr
                 | non_unary_print_expr non_unary_print_expr
                 | non_unary_print_expr '˜' print_expr
                 | non_unary_print_expr "!~" print_expr
                 | non_unary_print_expr IN NAME
                 | '(' multiple_expr_list ')' IN NAME
                 
                 | non_unary_print_expr "&&" newline_opt print_expr
                 | non_unary_print_expr "||" newline_opt print_expr
                 | non_unary_print_expr '?' print_expr ':' print_expr
                 | NUMBER
                 | STRING
                 | lvalue
                 | ERE
                 | lvalue "++"
                 | lvalue "--"
                 | "++" lvalue
                 | "--" lvalue
                 | lvalue "^=" print_expr
                 | lvalue "%=" print_expr
                 | lvalue "*=" print_expr
                 | lvalue "/=" print_expr
                 | lvalue "+=" print_expr
                 | lvalue "-=" print_expr
                 | lvalue '=' print_expr
                 | FUNC_NAME '(' expr_list_opt ')'
                 | BUILTIN_FUNC_NAME '(' expr_list_opt ')'
                 | BUILTIN_FUNC_NAME
                 ;

lvalue           : NAME
                 | NAME '[' expr_list ']'
                 | '$' expr
                 ;

non_unary_input_function : simple_get
                 | simple_get '<' expr
                 | non_unary_expr '|' simple_get
                 ;

unary_input_function : unary_expr '|' simple_get
                 ;

simple_get       : GETLINE
                 | GETLINE lvalue
                 ;

newline_opt      : /* empty */
                 | newline_opt NEWLINE
                 ;
p
