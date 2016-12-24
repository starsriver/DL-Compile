module FileIO

    let ReadSourceFile = fun fileName ->
        try
            let fs = new System.IO.FileStream(fileName, System.IO.FileMode.Open, System.IO.FileAccess.Read)
            let sr = new System.IO.StreamReader(fs)
            let mutable lineNum = 0
            let mutable str = sr.ReadLine()
            let mutable codeList = []
            while not (isNull str) do
                lineNum <- lineNum + 1
                let index1 = str.IndexOf "--"
                if index1 <> -1 then
                    str <- str.Remove index1
                let index2 = str.IndexOf "//"
                if index2 <> -1 then
                    str <- str.Remove index2
                if str.Length <> 0 then
                    codeList <- codeList @ [(str.Trim(), lineNum)]
                str <- sr.ReadLine()
            sr.Dispose()
            fs.Dispose()
            Some(codeList)
        with
            | _ -> raise (Type.ReadFileException(fileName))

    let buildSVGXML = fun (rectList:string list) ->
        let svgHead = "<?xml version=\"1.0\" standalone=\"no\"?>\n<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n<svg width=\"100%\" height=\"100%\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n"
        let svgTail = "</svg>"
        let rec connect = fun list ->
            match list with
                | [] -> ""
                | [_] -> list.Head
                | head::tail -> head + (connect tail)
        svgHead + (connect rectList) + svgTail

    let writeSVGFile = fun (fileName:string) (fileContent:string) ->
        try
            let fs = new System.IO.FileStream(fileName, System.IO.FileMode.Create, System.IO.FileAccess.Write)
            let wr = new System.IO.StreamWriter(fs, System.Text.Encoding.UTF8)
            wr.Write(fileContent)
            wr.Dispose()
            fs.Dispose()
            printfn "%s Is Ready" fileName
        with
        | _ -> raise (Type.WriteFileException(fileName))