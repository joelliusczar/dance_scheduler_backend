module Round
    open DanceDef
    open Teams
    type Round = 
        {
            Number: int
            DanceDef: DanceDef
            TeamEntries: List<TeamEntry>
            Impossible: Set<string>
        }
    
    let tryConsumeDance team round =
        let blockPerson1 = round.Impossible.Contains team.TeamDef.Person1.Name
        let blockPerson2 = round.Impossible.Contains team.TeamDef.Person2.Name
        let blockTeam = blockPerson1 || blockPerson2
        match blockTeam with
        | true -> (None, team)
        | false -> 
            let dance, team2 = consumeDance team round.DanceDef
            (dance, { team2 with LastRoundNumber = round.Number + 1 })

    let updateRoundWithTeam round team dance=
        let teamEntry = {
            Team = team
            Dance = dance
        }
        {
            round with
                Impossible = round.Impossible 
                |> Set.add team.Person1.Name 
                |> Set.add team.Person2.Name
                TeamEntries = teamEntry :: round.TeamEntries
        }

    let strFormatRound round =
        let header1 = $"Dance {round.DanceDef.FullName} Heat# {round.Number + 1}\n"
        let header2 = $"Leader|Follower|Age Group|Skill Level\n"
        let entries = 
            round.TeamEntries
            |> Seq.map (fun e -> 
                $"{e.Team.Person1.Name} | {e.Team.Person2.Name}"
            )
            |> String.concat "\n"
        $"{header1}{header2}{entries}\n"


    let strFormatAllRounds allRounds =
        allRounds
        |> Seq.map (fun r -> strFormatRound r)
        |> String.concat "\n"
    // let formatCsvRounds rounds =
    //     let header = $"Leader,Follower,Dance,AgeGroup,SkillLevel,HeatNumber\n"
    //     rounds 
    //     |> Seq.map (fun r -> )
        