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
    

        
    let rec collectRoundsForTeamEntries teamEntries personRoundMap =
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
                personRoundMap3
        | _ -> personRoundMap

    let rec roundsToPersonalRoundsMap allRounds personRoundMap =
        match allRounds with
        | round::t -> 
            let personRoundMap2 = 
                collectRoundsForTeamEntries 
                    round.TeamEntries
                    personRoundMap
            roundsToPersonalRoundsMap t personRoundMap2
        | _ -> personRoundMap
                
