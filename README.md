# Simply-typed lambda expressions type typechecker

## Grammar
    input       ::= [context '|-'] typed_expr

    context     ::= eps | variable ':' type [',' context]

    typed_expr  ::= expr ':' type

    type        ::= variable
                | (type)
                | (type) '->' type
                | variable '->' type

    expr        ::= [application] '\' variable '.' expr
                | application

    application ::= atom
                | application atom

    atom        ::= (expr)
                | variable

    variable    ::= [a-z] [a-z0-9'_]*
