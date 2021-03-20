module Parsers

open FParsec

open LangTypes

let pint = pint32 |>> (fun n -> Int(n))

let pString =
    let stringChars = manySatisfy (fun c -> c <> '\"')
    pchar '\"' >>. stringChars .>> pchar '\"'
    |>> fun x -> String(x)

//Non empty String that starts with a letter and can contain letters and digits
let pId = 
    let isIdFirstChar c = isLetter c || c = '_'
    let isIdChar c = isLetter c || isDigit c || c = '_'
    let p = many1Satisfy2 isIdFirstChar isIdChar
    p >>= (fun res -> if res = "while" || res = "fun" then fail $"{res} is reserved word" else parse.Return(res))

let pVar =
    pId 
    |>> fun x -> Variable(x)

let pValue = pint <|> pVar <|> pString

let pdatatype = 
    (pstring "int" |>> fun _ -> DatatypeInt) <|> (pstring "string" |>> fun _ -> DatatypeString)


//create reference to expr to make it available to mutually dependent definitions
//before it's defined
let pExpr, pExprRef = createParserForwardedToRef<Expr,unit>()

let pOp = choice [
    pstring "+" |>> fun _ -> Plus
    pstring "*" |>> fun _ -> Multiplication
    pstring "-" |>> fun _ -> Minus
]

let pCondOp = choice [
    pstring "<" |>> fun _ -> LESSTHAN
    pstring "<=" |>> fun _ -> LESSOREQUAL
    pstring ">" |>> fun _ -> GREATERTHAN
    pstring ">=" |>> fun _ -> GREATEROREQUAL
    pstring "==" |>> fun _ -> EQUAL
]

let pArithmetic =
    pValue .>>. pOp .>>. pValue
    |>> fun ((left,op),right) -> Arithmetic(left,op,right)

let pAssignment =
    let toAssignment optd name _ value =
        Assignment(optd, name, value)

    let dt = opt (pdatatype .>> spaces1)
    let equalSign = pstring "="
    pipe4 dt pId equalSign pExpr toAssignment

let pFunctionCall =
    let pParams = (pchar '(') >>. (sepBy pExpr (pchar ',')) .>> (pchar ')')
    (pId .>>. pParams)
    |>> fun (name,ps) -> FunctionCall(name, ps)


let pFunctionDeclaration =
    let pFun = pstring "fun" .>> spaces1
    let pParams = (pchar '(') >>. (sepBy pExpr (pchar ',')) .>> (pchar ')')
    let pBlock = (pchar '{') >>. (sepBy pExpr (pchar ';')) .>> (pchar '}')
    pFun >>. pId .>>. pParams .>>. pBlock
    |>> fun ((name,ps),exprs) -> FunctionDeclaration(name,ps,exprs)

let pReturn =
    pstring "return" >>. spaces1 >>. pExpr
    |>> fun x -> Return(x)

let pWhile =
    let toWhile ((a,b,c),exprs) =
        While(a,b,c, exprs)

    let pWhile = pstring "while"
    let pCondIn = 
        pExpr .>>. pCondOp .>>. pExpr
        |>> fun ((a,b),c) -> (a,b,c)

    let pCond = (pchar '(') >>. pCondIn .>> (pchar ')')
    let pBlock = (pchar '{') >>. (sepBy pExpr (pchar ';')) .>> (pchar '}')
    pWhile >>. pCond .>>. pBlock
    |>> toWhile


//replace dummy implementation with real one
pExprRef := choice [
    attempt pFunctionDeclaration
    attempt pWhile
    attempt pFunctionCall
    attempt pReturn
    attempt pArithmetic
    attempt pAssignment
    attempt pValue  
    pVar
]

//program is multiple statements seperated by spaces
let pProgram = sepBy pExpr spaces1