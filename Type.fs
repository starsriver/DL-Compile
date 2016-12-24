module Type

    let GlobalT = ref 0.00
    let GlobalWidth = ref 4.0
    let GlobalHeight = ref 4.0
    let GlobalRot = ref 0.0
    let GlobalOriginX = ref 0.0
    let GlobalOriginY = ref 0.0
    let GlobalTranslateX = ref 0.0
    let GlobalTranslateY = ref 0.0
    let GlobalScaleX = ref 1.0
    let GlobalScaleY = ref 1.0
    let GlobalFrom = ref 0.0
    let GlobalTo = ref 0.0
    let GlobalStep = ref 0.0

    type TokenType = | LeftParenthesis | RightParenthesis | Number | Function1 | Function2 | GlobalFunction1 | GlobalFunction2 | FunctionFor | Constant | T
    
    type TokenValue = 
        | Parenthesis of char
        | Number of float
        | Function of string
        | T of float ref

    type Token = {Type: TokenType; Value: TokenValue}

    let Function1 = set [|"sin" ; "cos" ; "tan" ; "sqrt"|]
    let Function2 = set [|"+" ; "-" ; "*" ; "/" ; "**"|]
    let GlobalFunction1 = set [|"width" ; "height" ; "rot"|]
    let GlobalFunction2 = set [|"origin" ; "translate" ; "scale"|]
    let FunctionFor = set [|"for" ; "from" ; "to" ; "step" ; "draw"|]
    let Constant = set [|"pi"|]
    let T = set [|"t"|]
    
    type NodeType = | Program | NumberLiteral | CallExpressions | VariableExpressions | ForExpressions | T | Empty

    type Node = {Type: NodeType; Value: TokenValue option; Name: TokenValue option; mutable Params: Node list option; ParamsNumber: int option}

    type VariableExpressionsOptions = | Static | RunTime

    exception ReadFileException of string
    exception WriteFileException of string
    exception TokenizerException of int*char
    exception ParserException of Token
    exception TransformationException of Node
    exception DrawException of string
    exception TokenValueException of TokenValue

    let TokenValueParenthesisChar = fun (tokenvalue: TokenValue) ->
        match tokenvalue with
            | TokenValue.Parenthesis char -> char
            | _ -> raise (TokenValueException(tokenvalue))

    let TokenValueNumberValue = fun (tokenvalue: TokenValue) ->
        match tokenvalue with
            | TokenValue.Number value -> value
            | _ -> raise (TokenValueException(tokenvalue))

    let TokenValueFunctionName = fun (tokenvalue: TokenValue) ->
        match tokenvalue with
            | TokenValue.Function name -> name
            | _ -> raise (TokenValueException(tokenvalue))

    let TokenValueTValue = fun (tokenvalue: TokenValue) ->
        match tokenvalue with
            | TokenValue.T valueRef -> !valueRef
            | _ -> raise (TokenValueException(tokenvalue))
            
    let showHelp = fun () ->
        printfn "\nDL Compile Tools (2016-12-24)"
        printfn "Usage: dotnet run [CodeFile] [Commands]\n"
        printfn "Arguments:"
        printfn "  [CodeFile]\t The Draw Language Source Code File"
        printfn "  [Commands]\t The Run Options\n"
        printfn "Run Options:"
        printfn "  -h\t Show Help"
        printfn "  -c\t Show The Processed Code Source"
        printfn "  -t\t Show The Token List"
        printfn "  -a\t Show The Primitive AST"
        printfn "  -n\t Show The Static Handle AST"
        printfn "  -r\t Show The Rect List"
        printfn "  -s\t Show The SVG File Content"
        printfn "  -o\t Build The SVG File\n\n"