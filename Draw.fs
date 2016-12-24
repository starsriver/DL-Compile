module Draw
    let drawRect = fun (x:float, y:float) ->
        "<rect x=\"" + x.ToString() + "\" " + "y=\"" + y.ToString() + "\" width=\"" + (!Type.GlobalWidth).ToString() + "\" height=\"" + (!Type.GlobalHeight).ToString() + "\"/>\n"

    let translatePoint = fun x y ->
        let scaleX = x * !Type.GlobalScaleX
        let scaleY = y * !Type.GlobalScaleY 
        let rotX = scaleX * System.Math.Cos(!Type.GlobalRot) + scaleY * System.Math.Sin(!Type.GlobalRot)
        let rotY = scaleY * System.Math.Cos(!Type.GlobalRot) - scaleX * System.Math.Sin(!Type.GlobalRot)
        let originX = rotX + !Type.GlobalOriginX + !Type.GlobalTranslateX
        let originY = rotY + !Type.GlobalOriginY + !Type.GlobalTranslateY
        (originX, originY)

    let drawExpression = fun (node: Type.Node) ->
        let mutable pointList = List.empty<float*float>
        match node.Type with
            | Type.NodeType.CallExpressions ->
                match node.Name.Value with
                    | Type.TokenValue.Function("width") ->
                        Type.GlobalWidth := node.Params.Value.[0].Value.Value |> Type.TokenValueNumberValue
                    | Type.TokenValue.Function("height") ->
                        Type.GlobalHeight := node.Params.Value.[0].Value.Value |> Type.TokenValueNumberValue
                    | Type.TokenValue.Function("rot") ->
                        Type.GlobalRot := node.Params.Value.[0].Value.Value |> Type.TokenValueNumberValue
                    | Type.TokenValue.Function("origin") ->
                        Type.GlobalOriginX := node.Params.Value.[0].Value.Value |> Type.TokenValueNumberValue
                        Type.GlobalOriginY := node.Params.Value.[1].Value.Value |> Type.TokenValueNumberValue
                    | Type.TokenValue.Function("translate") ->
                        Type.GlobalTranslateX := node.Params.Value.[0].Value.Value |> Type.TokenValueNumberValue
                        Type.GlobalTranslateY := node.Params.Value.[1].Value.Value |> Type.TokenValueNumberValue
                    | Type.TokenValue.Function("scale") ->
                        Type.GlobalScaleX := node.Params.Value.[0].Value.Value |> Type.TokenValueNumberValue
                        Type.GlobalScaleY := node.Params.Value.[1].Value.Value |> Type.TokenValueNumberValue
                    | _ ->
                        raise (Type.DrawException("Unknow " + node.Name.Value.ToString()))
            | Type.NodeType.ForExpressions ->
                Type.GlobalFrom := node.Params.Value.[1].Value.Value |> Type.TokenValueNumberValue
                let f = !Type.GlobalFrom
                Type.GlobalTo := node.Params.Value.[2].Value.Value |> Type.TokenValueNumberValue
                let t = !Type.GlobalTo
                Type.GlobalStep := node.Params.Value.[3].Value.Value |> Type.TokenValueNumberValue
                let s = !Type.GlobalStep
                Type.GlobalT := !Type.GlobalFrom
                while !Type.GlobalT <= !Type.GlobalTo do
                    let xNode = Transformation.transformationVariableExpressionsOptions node.Params.Value.[4] Type.VariableExpressionsOptions.RunTime
                    let yNode = Transformation.transformationVariableExpressionsOptions node.Params.Value.[5] Type.VariableExpressionsOptions.RunTime
                    let point = translatePoint (xNode.Value.Value |> Type.TokenValueNumberValue) (yNode.Value.Value |> Type.TokenValueNumberValue)
                    pointList <- pointList @ [point]
                    Type.GlobalT := !Type.GlobalT + !Type.GlobalStep
            | _ -> 
                raise (Type.DrawException("Unknow " + node.Type.ToString()))
        pointList

    let drawPoint = fun (rootNode: Type.Node) ->
        if rootNode.Type = Type.NodeType.Program then
            List.collect drawExpression rootNode.Params.Value
        else
            raise (Type.DrawException("Unknow rootNode"))

    let draw = fun (rootNode: Type.Node) ->
        let pointList = drawPoint rootNode
        List.map drawRect pointList
