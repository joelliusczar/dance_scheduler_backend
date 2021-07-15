// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System 
open System.IO
open DSUtilities
open DanceDef
open ImportData
open Round
open RoundBuilder
open ResultsPrinter

let defsFileName = "Dance Comp - Definitions.csv"
let fileName = "Dance Comp - Entries.csv"
try
    let compDefs = using(new StreamReader(File.OpenRead(defsFileName))) (fun f ->
        printfn "Building defs"
        f.ReadLine() |> ignore //swollowing the label row
        buildDefsFromStream f
    )

    printfn "Building defs"
    try
        using(new StreamReader(File.OpenRead(fileName))) (fun f ->
            //1 + 1
            printfn "About to buld teams"
            f.ReadLine() |> ignore //swollowing the label row
            let _, teams, _ = 
                buildDataFromStream 
                    f 
                    compDefs 
                    Map.empty 
                    Map.empty 
                    List.empty
            match teams with
            | Some teams ->
                printfn "Teams built"
                let allTeams =
                    teams
                    |> Seq.map (fun kvp -> kvp.Value) 
                    |> Seq.toList
                printfn "About to buld rounds"
                let listOfRoundList = 
                    buildComp 
                        compDefs.DanceDefs 
                        compDefs.RankDefs 
                        allTeams 
                        |> Seq.toList
                printfn "About to print rounds"
                do listOfRoundList 
                    |> Seq.concat
                    |> Seq.iteri (fun idx round ->

                        // let output = strFormatAllRounds formatCsvRound roundList 
                        // printfn "%s" output
                        try
                            let newFile = idx < 1
                            using(new StreamWriter("heats.csv", not newFile)) (fun f ->
                                let line = formatCsvRound compDefs round (idx + 1)
                                f.WriteLine(line)
                            )
                        with
                            | _ -> printfn "An error happened while outputting results"
                    )
                printfn "build scores"
                try
                    using(new StreamWriter("scores.csv", false)) (fun f ->
                        f.WriteLine(scoreHeader)
                        do listOfRoundList 
                            |> Seq.concat
                            |> Seq.iteri (fun idx round ->

                                // let output = strFormatAllRounds formatCsvRound roundList 
                                // printfn "%s" output
            
                                let line = formatScoreRound compDefs round (idx + 1)
                                f.WriteLine(line)
                            )
                    )
                with
                    | _ -> printfn "An error happened while outputting results"
            | None -> printfn "Ended early"
        )
    with
        | :? System.IO.FileNotFoundException -> printfn "%s was not there" fileName
with
    | :? System.IO.FileNotFoundException -> printfn "%s was not there" defsFileName


[<EntryPoint>]
let main argv =
    //printfn "%A" somelist
    //printfn "%A" removed
    // let connections = linkPeople Map.empty person1 person2
    // printfn "%A" connections
    //danceLookup |> Seq.iter (fun x -> printfn "%A" x)
    0 // return an integer exit code