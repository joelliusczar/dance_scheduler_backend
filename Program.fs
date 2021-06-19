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
    let danceDefs, rankDefs = using(new StreamReader(File.OpenRead(defsFileName))) (fun f ->
        printfn "Building defs"
        buildDefsFromStream f
    )

    printfn "Building defs"
    try
        using(new StreamReader(File.OpenRead(fileName))) (fun f ->
            //1 + 1
            printfn "About to buld teams"
            let _, teams = buildDataFromStream f danceDefs Map.empty Map.empty
            printfn "Teams built"
            let allTeams =
                teams
                |> Seq.map (fun kvp -> kvp.Value) 
                |> Seq.toList
            printfn "About to buld rounds"
            let listOfRoundList = 
                buildComp 
                    danceDefs 
                    rankDefs 
                    allTeams 
                    |> Seq.toList
            do listOfRoundList 
                |> Seq.iteri (fun idx roundList ->

                    // let output = strFormatAllRounds formatCsvRound roundList 
                    // printfn "%s" output
                    try
                        let newFile = idx < 1
                        using(new StreamWriter("heats.csv", not newFile)) (fun f ->
                            for line in strFormatAllRounds formatCsvRound roundList do
                                f.WriteLine(line)
                        )
                    with
                        | _ -> printfn "An error happened while outputting results"
                )
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