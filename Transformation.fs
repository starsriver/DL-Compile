module Transformation

    let rec transformationVariableExpressionsOptions = fun (node : Type.Node) (option: Type.VariableExpressionsOptions) ->
        if node.Name.IsNone then
            match node.Type with
                | Type.NodeType.T ->
                    let value = node.Value.Value |> Type.TokenValueTValue
                    {Type.Node.Type = Type.NodeType.NumberLiteral; Type.Node.Name = None; Type.Node.Value = Some(Type.TokenValue.Number(value)); Type.Node.Params = None; Type.Node.ParamsNumber = None}
                | Type.NodeType.NumberLiteral ->
                    node
                | _ -> 
                    raise (Type.TransformationException(node))
        else 
            let mutable current = 0
            let mutable (newParams:Type.Node list) = []
            while current < node.ParamsNumber.Value do
                newParams <- newParams @ [transformationVariableExpressionsOptions node.Params.Value.[current] option]
                current <- current + 1
            if option = Type.VariableExpressionsOptions.Static then
                node.Params <- Some(newParams)
            match node.Name.Value with
                | Type.TokenValue.Function("Parenthesis") ->
                    let node = transformationVariableExpressionsOptions newParams.[0] option
                    node
                | Type.TokenValue.Function("sin") ->
                    let value = newParams.[0].Value.Value |> Type.TokenValueNumberValue |> System.Math.Sin
                    {Type = Type.NodeType.NumberLiteral; Name = None; Value = Some(Type.TokenValue.Number(value)); Params = None; ParamsNumber = None}
                | Type.TokenValue.Function("cos") ->
                    let value = newParams.[0].Value.Value |> Type.TokenValueNumberValue |> System.Math.Cos
                    {Type = Type.NodeType.NumberLiteral; Name = None; Value = Some(Type.TokenValue.Number(value)); Params = None; ParamsNumber = None}
                | Type.TokenValue.Function("tan") ->
                    let value = newParams.[0].Value.Value |> Type.TokenValueNumberValue |> System.Math.Tan
                    {Type = Type.NodeType.NumberLiteral; Name = None; Value = Some(Type.TokenValue.Number(value)); Params = None; ParamsNumber = None}
                | Type.TokenValue.Function("sqrt") ->
                    let value = newParams.[0].Value.Value |> Type.TokenValueNumberValue |> System.Math.Sqrt
                    {Type = Type.NodeType.NumberLiteral; Name = None; Value = Some(Type.TokenValue.Number(value)); Params = None; ParamsNumber = None}
                | Type.TokenValue.Function("+") ->
                    let value1 = newParams.[0].Value.Value |> Type.TokenValueNumberValue
                    let value2 = newParams.[1].Value.Value |> Type.TokenValueNumberValue
                    {Type = Type.NodeType.NumberLiteral; Name = None; Value = Some(Type.TokenValue.Number(value1 + value2)); Params = None; ParamsNumber = None}
                | Type.TokenValue.Function("-") ->
                    let value1 = newParams.[0].Value.Value |> Type.TokenValueNumberValue
                    let value2 = newParams.[1].Value.Value |> Type.TokenValueNumberValue
                    {Type = Type.NodeType.NumberLiteral; Name = None; Value = Some(Type.TokenValue.Number(value1 - value2)); Params = None; ParamsNumber = None}
                | Type.TokenValue.Function("*") ->
                    let value1 = newParams.[0].Value.Value |> Type.TokenValueNumberValue
                    let value2 = newParams.[1].Value.Value |> Type.TokenValueNumberValue
                    {Type = Type.NodeType.NumberLiteral; Name = None; Value = Some(Type.TokenValue.Number(value1 * value2)); Params = None; ParamsNumber = None}
                | Type.TokenValue.Function("/") ->
                    let value1 = newParams.[0].Value.Value |> Type.TokenValueNumberValue
                    let value2 = newParams.[1].Value.Value |> Type.TokenValueNumberValue
                    {Type = Type.NodeType.NumberLiteral; Name = None; Value = Some(Type.TokenValue.Number(value1 / value2)); Params = None; ParamsNumber = None}
                | Type.TokenValue.Function("**") ->
                    let value1 = newParams.[0].Value.Value |> Type.TokenValueNumberValue
                    let value2 = newParams.[1].Value.Value |> Type.TokenValueNumberValue
                    let value = System.Math.Pow(value1, value2)
                    {Type = Type.NodeType.NumberLiteral; Name = None; Value = Some(Type.TokenValue.Number(value)); Params = None; ParamsNumber = None}
                | _ -> 
                    raise (Type.TransformationException(node))

    let transformationCallExpressions = fun (node : Type.Node) ->
        let mutable current = 0
        let mutable (newParams:Type.Node list) = []
        while current < node.ParamsNumber.Value do
            newParams <- newParams @ [transformationVariableExpressionsOptions node.Params.Value.[current] Type.VariableExpressionsOptions.Static]
            current <- current + 1
        node.Params <- Some(newParams)
        node

    let transformationForExpressions = fun (node : Type.Node) ->
        let mutable current = 1
        let mutable (newParams:Type.Node list) = []
        newParams <- newParams @ [node.Params.Value.[0]]
        while current < 4 do
            newParams <- newParams @ [transformationVariableExpressionsOptions node.Params.Value.[current] Type.VariableExpressionsOptions.Static]
            current <- current + 1
        newParams <- newParams @ [node.Params.Value.[4]]
        newParams <- newParams @ [node.Params.Value.[5]]
        node.Params <- Some(newParams)
        node
    
    let transformationProgram = fun (node : Type.Node) ->
        let rootNode = {Type.Node.Type= Type.NodeType.Program; Type.Node.Value = None; Type.Node.Name = None; Type.Node.Params = Some([]); Type.Node.ParamsNumber = None}
        for expression in node.Params.Value do
            match expression.Type with
                | Type.NodeType.CallExpressions ->
                    rootNode.Params <- Some(rootNode.Params.Value @ [expression |> transformationCallExpressions])
                | Type.NodeType.ForExpressions ->
                    rootNode.Params <- Some(rootNode.Params.Value @ [expression |> transformationForExpressions])
                | _ ->
                    raise (Type.TransformationException(node))
        rootNode