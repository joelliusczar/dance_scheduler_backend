module ResultsPrinter
  open Round
  open Teams

  let strFormatRound round =
    let header1 = $"Dance {round.DanceDef.FullName} Heat# {round.Number + 1}\n"
    let header2 = $"Leader | Follower | Age Group | Skill Level\n"
    let entries = 
        round.TeamEntries
        |> Seq.map (fun e -> 
            $"{e.Team.Person1.Name} | {e.Team.Person2.Name} | {e.Dance.AgeGroup} | {e.Dance.SkillLevl}"
        )
        |> String.concat "\n"
    $"{header1}{header2}{entries}\n"

  let strFormatAllRounds rowFormatter allRounds =
    allRounds
    |> Seq.map (fun r -> rowFormatter r)

  let formatCsvRound round =
      //let header = $"Leader,Follower,Dance,AgeGroup,SkillLevel,HeatNumber\n"
    let entries = 
      round.TeamEntries
      |> Seq.map (fun e -> 
        $"\"{e.Team.Person1.Name}\",\"{e.Team.Person2.Name}\",\"{e.Dance.DanceDef.FullName}\",\"{e.Dance.AgeGroup}\",\"{e.Dance.SkillLevl}\",\"{round.Number}\""
      )
      |> String.concat "\n"
    entries

  let strFormatPersonRoundSheet personName teamEntries = 
    let top = $"Heat Sheet for {personName}"
    let lines = 
        teamEntries
        |> Seq.map(fun e ->
            let teamMateName = 
                match selectTeamMate personName e with
                | None -> "N/A"
                | Some p -> p.Name
            $"Heat #{e.RoundNumber} {e.Dance.DanceDef.FullName} {e.Dance.AgeGroup} | {e.Dance.SkillLevl} | {teamMateName}"
        )
        |> String.concat "\n"
    $"${lines}"

        // let strFormatAllRoundSheets personRoundMap =
        //     personRoundMap
        //     |> Seq.map (fun kvp -> strFormatPersonRoundSheet kvp)