module Parser

    let lineParser = fun (tokens:Type.Token list) ->
        let mutable current = 0
        let mutable token = tokens.[current]
        let mutable GlobalNode = {Type.Node.Type = Type.NodeType.Empty; Type.Node.Value = None; Type.Node.Name = None; Type.Node.Params = None; Type.Node.ParamsNumber = None}

        let NextToken = fun(token':Type.Token) ->
            if token' = tokens.[current] then
                if current + 1 >= tokens.Length then
                    ()
                else    
                    current <- current + 1
                    token <- tokens.[current]
            else
                raise (Type.ParserException(token))

        let rec Expression = fun() ->
            Term()
            while token.Value = Type.TokenValue.Function("+") || token.Value = Type.TokenValue.Function("-") do
                let node = {Type.Node.Type = Type.NodeType.VariableExpressions; Type.Node.Value = None; Type.Node.Name = Some(token.Value); Type.Node.Params = Some([]); Type.Node.ParamsNumber = Some(2)}
                node.Params <- Some([GlobalNode])
                GlobalNode <- node
                NextToken token
                Term()
                node.Params <- Some(node.Params.Value @ [GlobalNode])
                GlobalNode <- node

        and Term = fun() ->
            Factor()
            while token.Value = Type.TokenValue.Function("*") || token.Value = Type.TokenValue.Function("/") || token.Value = Type.TokenValue.Function("%") do
                let node = {Type.Node.Type = Type.NodeType.VariableExpressions; Type.Node.Value = None; Type.Node.Name = Some(token.Value); Type.Node.Params = Some([]); Type.Node.ParamsNumber = Some(2)}
                node.Params <- Some([GlobalNode])
                GlobalNode <- node
                NextToken token
                Factor()
                node.Params <- Some(node.Params.Value @ [GlobalNode])
                GlobalNode <- node

        and Factor = fun() ->
            if token.Value = Type.TokenValue.Function("+") || token.Value = Type.TokenValue.Function("-") then
                let node = {Type.Node.Type = Type.NodeType.VariableExpressions; Type.Node.Value = None; Type.Node.Name = Some(token.Value); Type.Node.Params = Some([]); Type.Node.ParamsNumber = Some(2)}
                let tempNode = {Type.Node.Type = Type.NodeType.NumberLiteral; Type.Node.Value = Some(Type.TokenValue.Number(0.0)); Type.Node.Name = None; Type.Node.Params = None; Type.Node.ParamsNumber = None}
                node.Params <- Some([tempNode])
                NextToken token
                Expression()
                node.Params <- Some(node.Params.Value @ [GlobalNode])
                GlobalNode <- node
                NextToken token
            else
                Component()

        and Component = fun() ->
            Atom()
            if token.Value = Type.TokenValue.Function("**") then
                let node = {Type.Node.Type = Type.NodeType.VariableExpressions; Type.Node.Value = None; Type.Node.Name = Some(token.Value); Type.Node.Params = Some([]); Type.Node.ParamsNumber = Some(2)}
                node.Params <- Some([GlobalNode])
                NextToken token
                Component()
                node.Params <- Some(node.Params.Value @ [GlobalNode])
                GlobalNode <- node

        and Atom = fun() ->
            match token.Type with
                | Type.TokenType.T ->
                    let node = {Type.Node.Type = Type.NodeType.T; Type.Node.Value = Some(token.Value); Type.Node.Name = None; Type.Node.Params = None; Type.Node.ParamsNumber = None}
                    GlobalNode <- node
                    NextToken token
                | Type.TokenType.Number ->
                    let node = {Type.Node.Type = Type.NodeType.NumberLiteral; Type.Node.Value = Some(token.Value); Type.Node.Name = None; Type.Node.Params = None; Type.Node.ParamsNumber = None}
                    GlobalNode <- node
                    NextToken token
                | Type.TokenType.Function1 ->
                    let node = {Type.Node.Type = Type.NodeType.VariableExpressions; Type.Node.Value = None; Type.Node.Name = Some(token.Value); Type.Node.Params = Some([]); Type.Node.ParamsNumber = Some(1)}
                    NextToken token
                    Expression()
                    node.Params <- Some([GlobalNode])
                    GlobalNode <- node
                | Type.TokenType.Function2 ->
                    let node = {Type.Node.Type = Type.NodeType.VariableExpressions; Type.Node.Value = None; Type.Node.Name = Some(token.Value); Type.Node.Params = Some([]); Type.Node.ParamsNumber = Some(2)}
                    NextToken token
                    Expression()
                    node.Params <- Some([GlobalNode])
                    Expression()
                    node.Params <- Some(node.Params.Value @ [GlobalNode])
                    GlobalNode <- node
                | Type.TokenType.GlobalFunction1 ->
                    let node = {Type.Node.Type = Type.NodeType.CallExpressions; Type.Node.Value = None; Type.Node.Name = Some(token.Value); Type.Node.Params = Some([]); Type.Node.ParamsNumber = Some(1)}
                    NextToken token
                    Expression()
                    node.Params <- Some([GlobalNode])
                    GlobalNode <- node
                | Type.TokenType.GlobalFunction2 ->
                    let node = {Type.Node.Type = Type.NodeType.CallExpressions; Type.Node.Value = None; Type.Node.Name = Some(token.Value); Type.Node.Params = Some([]); Type.Node.ParamsNumber = Some(2)}
                    NextToken token
                    Expression()
                    node.Params <- Some([GlobalNode])
                    Expression()
                    node.Params <- Some(node.Params.Value @ [GlobalNode])
                    GlobalNode <- node
                | Type.TokenType.FunctionFor ->
                    let node = {Type.Node.Type = Type.NodeType.ForExpressions; Type.Node.Value = None; Type.Node.Name = Some(token.Value); Type.Node.Params = Some([]); Type.Node.ParamsNumber = Some(6)}
                    NextToken token
                    Atom()
                    if GlobalNode.Type = Type.NodeType.T then
                        node.Params <- Some([GlobalNode])
                    else
                        raise (Type.ParserException(token))
                    NextToken {Type.Token.Type = Type.TokenType.FunctionFor; Type.Token.Value = Type.TokenValue.Function("from")}
                    Expression()
                    node.Params <- Some(node.Params.Value @ [GlobalNode])
                    NextToken {Type.Token.Type = Type.TokenType.FunctionFor; Type.Token.Value = Type.TokenValue.Function("to")}
                    Expression()
                    node.Params <- Some(node.Params.Value @ [GlobalNode])
                    NextToken {Type.Token.Type = Type.TokenType.FunctionFor;Type.Token. Value = Type.TokenValue.Function("step")}
                    Expression()
                    node.Params <- Some(node.Params.Value @ [GlobalNode])
                    NextToken {Type.Token.Type = Type.TokenType.FunctionFor; Type.Token.Value = Type.TokenValue.Function("draw")}
                    Expression()
                    node.Params <- Some(node.Params.Value @ [GlobalNode])
                    Expression()
                    node.Params <- Some(node.Params.Value @ [GlobalNode])
                    GlobalNode <- node
                | Type.TokenType.LeftParenthesis ->
                    let node = {Type.Node.Type = Type.NodeType.VariableExpressions; Type.Node.Value = None; Type.Node.Name = Some(Type.TokenValue.Function("Parenthesis")); Type.Node.Params = Some([]); Type.Node.ParamsNumber = Some(1)}
                    NextToken token
                    Expression()
                    node.Params <- Some([GlobalNode])
                    GlobalNode <- node
                    NextToken {Type.Token.Type = Type.TokenType.RightParenthesis; Type.Token.Value = Type.TokenValue.Parenthesis(')')}
                | _ ->
                     raise (Type.ParserException(token))
        Expression()          
        GlobalNode

    let parser = fun (input:Type.Token list list) ->
        let rootNode = {Type.Node.Type= Type.NodeType.Program; Type.Node.Value = None; Type.Node.Name = None; Type.Node.Params = Some([]); Type.Node.ParamsNumber = None}
        let mutable current = 0
        while current < input.Length do
            rootNode.Params <- Some(rootNode.Params.Value @ [lineParser input.[current]])
            current <- current + 1
        rootNode