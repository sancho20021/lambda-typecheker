# Simply-typed lambda expressions type typechecker

## Run instructions
0. Make sure you have [cabal-install](https://cabal.readthedocs.io/en/latest/developing-packages.html) installed
1. Go to [./simply-typed/](https://github.com/sancho20021/lambda-typecheker/tree/main/simply-typed) directory
2. In the command line type the desired instruction:
    - ```make run``` - run the typecheker
    - ```make runShow``` - run the typecheker with showing errors
    - ```make test``` - run tests for the typechecker
    - ```make clean``` - clean generated build directory

## Grammar
    input       ::= [context '|-'] typed_expr

    context     ::= eps | variable ':' type [',' context]

    typed_expr  ::= expr ':' type

    type        ::= variable
                | (type)
                | (type) '->' type
                | variable '->' type

    expr        ::= [application] '\' variable ':' type '.' expr
                | application

    application ::= atom
                | application atom

    atom        ::= (expr)
                | variable

    variable    ::= [a-z] [a-z0-9'_]*
