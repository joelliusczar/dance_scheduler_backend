module Round
    open DanceDef
    open Dance
    open Person
    open Teams
    open DSUtilities
    
    let update = Map.add

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
            RoundNumber = round.Number
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
        let header2 = $"Leader | Follower | Age Group | Skill Level\n"
        let entries = 
            round.TeamEntries
            |> Seq.map (fun e -> 
                $"{e.Team.Person1.Name} | {e.Team.Person2.Name} | {e.Dance.AgeGroup} | {e.Dance.SkillLevl}"
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

    let selectPartner personName teamEntry = 
        match personName with
        | p when p = teamEntry.Team.Person1 -> Some teamEntry.Team.Person2
        | p when p = teamEntry.Team.Person2 -> Some teamEntry.Team.Person1
        | _ -> None

    let strFormatPersonRoundSheet personalTeamEntries = 
        let entries = 
            personalTeamEntries
            |> Seq.map(fun e -> 
                $"Heat #{e.RoundNumber} {e.Dance.DanceDef.FullName} {e.Dance.AgeGroup} | {e.Dance.SkillLevl}"
            )
            |> String.concat "\n"
        $"${entries}"



        
    let rec collectRoundsForTeamEntries teamEntries round personRoundMap =
        match teamEntries with
        | teamEntry::rest ->
            let person1Name = teamEntry.Team.Person1.Name
            let person1Entries = findOrDefaultList personRoundMap person1Name
            let expandedPerson1 = teamEntry::person1Entries
            
            let person2Name = teamEntry.Team.Person2.Name
            let person2Entries = findOrDefaultList personRoundMap person2Name
            let expandedPerson2 = teamEntry::person2Entries

            let personRoundMap2 = personRoundMap |> update person1Name expandedPerson1
            let personRoundMap3 = personRoundMap2 |> update person2Name expandedPerson2
            collectRoundsForTeamEntries 
                rest 
                round 
                personRoundMap3
        | _ -> personRoundMap

    let rec roundsToPersonalRoundsMap allRounds personRoundMap =
        match allRounds with
        | h::t -> 
            let personRoundMap2 = 
                collectRoundsForTeamEntries 
                    h 
                    personRoundMap
            roundsToPersonalRoundsMap t personRoundMap2
        | _ -> personRoundMap
                
