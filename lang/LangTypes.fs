module LangTypes

type DataType =
    | DatatypeInt
    | DatatypeString

type ArithmeticOperator =
    | Plus
    | Minus
    | Multiplication
    | Division

type PredicateOperator =
    | LESSTHAN 
    | LESSOREQUAL
    | GREATERTHAN
    | GREATEROREQUAL
    | EQUAL

type Expr =
    | String of string
    | Int of int
    | Variable of string
    | Assignment of DataType option * string * Expr
    | FunctionCall of string * Expr list
    | FunctionDeclaration of string * Expr list * Expr list
    | While of Expr * PredicateOperator * Expr * Expr list
    | Arithmetic of Expr * ArithmeticOperator * Expr
    | Return of Expr
