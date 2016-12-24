module Tokenizer
   
    let isNumberChar = function char -> char >= '0' && char <= '9'
    
    let isIdentifierChar = function char -> (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z') || char = '_'
    
    let isOperatorChar = function char -> Type.Function2.Contains (char.ToString())
    
    let lineTotokenizer = fun (input:string, lineNum: int) ->
        let mutable current = 0
        let mutable (tokens: Type.Token list) = []
        while current < input.Length do 
            let mutable char = input.[current];
            match char with
                | '(' -> 
                    tokens <- tokens @ [{Type = Type.TokenType.LeftParenthesis; Value = Type.TokenValue.Parenthesis(char)}]
                    current <- current + 1
                | ')' -> 
                    tokens <- tokens @ [{Type = Type.TokenType.RightParenthesis; Value = Type.TokenValue.Parenthesis(char)}]
                    current <- current + 1
                | ' ' | '\t' ->
                    current <- current + 1
                | _ when isNumberChar char ->
                    let mutable value = ""
                    let mutable isOver = false
                    let mutable hasDot = false
                    while current < input.Length && not isOver do
                        char <- input.[current] 
                        if isNumberChar char || char = '.' then
                            if char = '.' then
                                if not hasDot && current + 1 < input.Length && isNumberChar input.[current + 1] then
                                    hasDot <- true
                                    value <- value + char.ToString()
                                    current <- current + 1
                                else
                                    raise (Type.TokenizerException(lineNum, char))
                            else
                                value <- value + char.ToString()
                                current <- current + 1
                        else
                            isOver <- true
                    let num = ref 0.0
                    if System.Double.TryParse(value, num) then
                        tokens <- tokens @ [{Type = Type.TokenType.Number; Value = Type.TokenValue.Number(!num)}]
                    else
                        raise (Type.TokenizerException(lineNum, char))
                | _ when isOperatorChar char ->
                    let mutable value = char.ToString()
                    if char = '*' then
                        if current + 1 < input.Length && input.[current + 1] = '*' then 
                            current <- current + 1
                            value <- value + input.[current].ToString()
                    tokens <- tokens @ [{Type = Type.TokenType.GlobalFunction2; Value = Type.TokenValue.Function(value)}]
                    current <- current + 1
                | _ when isIdentifierChar char ->
                    let mutable value = ""
                    let mutable isOver = false
                    while current < input.Length && not isOver do
                        char <- input.[current]
                        if isIdentifierChar char || isNumberChar char then
                            value <- value + char.ToString()
                            current <- current + 1
                        else
                            isOver <- true
                    if Type.T.Contains value then
                        tokens <- tokens @ [{Type = Type.TokenType.T; Value = Type.TokenValue.T(Type.GlobalT)}]
                    elif Type.Constant.Contains value then 
                        tokens <- tokens @ [{Type = Type.TokenType.Number; Value = Type.TokenValue.Number(System.Math.PI)}]
                    elif Type.Function1.Contains value then 
                        tokens <- tokens @ [{Type = Type.TokenType.Function1; Value = Type.TokenValue.Function(value)}]
                    elif Type.Function2.Contains value then 
                        tokens <- tokens @ [{Type = Type.TokenType.Function2; Value = Type.TokenValue.Function(value)}]
                    elif Type.GlobalFunction1.Contains value then 
                        tokens <- tokens @ [{Type = Type.TokenType.GlobalFunction1; Value = Type.TokenValue.Function(value)}]
                    elif Type.GlobalFunction2.Contains value then 
                        tokens <- tokens @ [{Type = Type.TokenType.GlobalFunction2; Value = Type.TokenValue.Function(value)}]
                    elif Type.FunctionFor.Contains value then 
                        tokens <- tokens @ [{Type = Type.TokenType.FunctionFor; Value = Type.TokenValue.Function(value)}]
                    else
                        raise (Type.TokenizerException(lineNum, char))
                | _ -> 
                    raise (Type.TokenizerException(lineNum, char))
        tokens

    let tokenizer = fun (input :(string*int) list option)->
        if input.IsNone then
            let (temp: Type.Token list list) = []
            temp
        else
            List.map lineTotokenizer input.Value