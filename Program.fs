[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    try 
        if argv.Length = 0 then 
            raise (System.ArgumentException("No Input File"))
        else
            let fileName = argv.[0]
            if not (fileName.EndsWith ".dl") then 
                raise (System.ArgumentException("Input File Is Not DL File"))
            else
                if (Array.tryFind (fun x -> x = "-h") argv).IsSome then
                    Type.showHelp()
                    
                let code = FileIO.ReadSourceFile fileName
                if (Array.tryFind (fun x -> x = "-c") argv).IsSome then
                    printfn "The code:\n%A\n" code

                let tokens = Tokenizer.tokenizer code
                if (Array.tryFind (fun x -> x = "-t") argv).IsSome then
                    printfn "The Tkoens:\n%A\n" tokens

                let ast = Parser.parser tokens
                if (Array.tryFind (fun x -> x = "-a") argv).IsSome then
                    printfn "The AST:\n%A\n" ast

                let newast = Transformation.transformationProgram ast
                if (Array.tryFind (fun x -> x = "-n") argv).IsSome then
                    printfn "The New AST:\n%A\n" newast

                let rectList = Draw.draw newast
                if (Array.tryFind (fun x -> x = "-r") argv).IsSome then
                    printfn "The RectList:\n%A\n" rectList

                let svgContent = FileIO.buildSVGXML rectList
                if (Array.tryFind (fun x -> x = "-s") argv).IsSome then
                    printfn "The SVG Content:\n%A\n" svgContent

                let newFileName = fileName.Replace(".dl", ".svg")
                if (Array.tryFind (fun x -> x = "-o") argv).IsSome then
                    FileIO.writeSVGFile "test.svg" svgContent
    with
        | Type.ReadFileException fileName-> 
            printfn "Can't get source code in %s file\n" fileName
        | Type.WriteFileException fileName-> 
            printfn "Can't write %s file!\n" fileName
        | Type.TokenizerException(lineNum, char) ->
            printfn "Line:%d\tCan't recognized %c\n" lineNum char
        | Type.ParserException token ->
            printfn "Can't recognise the Token:\n%A\n" token
        | Type.TransformationException node ->
            printfn "Can't transformation the Node:\n%A\n" node
        | Type.DrawException message ->
            printfn "Draw: %s\n" message
        | Type.TokenValueException tokenValue ->
            printfn "Can't Convert TokenValue To Value: %A\n" tokenValue
        | :?System.ArgumentException as e ->
            printfn "Argument Error: %s in %s\n" e.Message e.Source
        | :?System.NullReferenceException as e ->
            printfn "Reference Error: %s in %s\n" e.Message e.Source
        | e -> 
            printfn "Unknow Error:%s in %s\n"  e.Message e.Source
    0
