module Teams
    open DSUtilities
    open DanceDef
    open Dance
    open Person
    
    let update = Map.add


    type TeamDef =
        {
            Person1: Person
            Person2: Person
        }

    type Team = 
        {
            Dances: Map<string, list<Dance>>
            TeamDef: TeamDef
            TeamNumber: int
            LastRoundNumber: int
        }


    type TeamEntry = 
        {
            RoundNumber: int
            Team: TeamDef
            Dance: Dance
        }

    let updateTeamWithDance team dance =
        let entries = findOrDefaultList team.Dances dance.DanceDef.Name
        let expanded = dance :: entries
        { team with Dances = team.Dances |> update dance.DanceDef.Name expanded }

    let consumeDance team (danceDef: DanceDef) =
        let entries = findOrDefaultList team.Dances danceDef.Name
        match Seq.tryFind (fun d -> d.DanceDef = danceDef) entries with
        | None -> (None, team)
        | Some dance -> 
            let shrunk = entries |> removeN 1 (fun d -> d = dance)
            (
                Some dance, 
                { team with Dances = team.Dances |> update danceDef.Name shrunk }
            )

    let updateTeamWithoutDance team (danceDef: DanceDef) =
        let entries = findOrDefaultList team.Dances danceDef.Name
        let shrunk = entries |> removeN 1 (fun d -> d.DanceDef = danceDef)
        { team with Dances = team.Dances |> update danceDef.Name shrunk }

    let getDanceCount team (dance: DanceDef) =
        let count = entryCount team.Dances dance.Name
        let fitCount = 
            seq { 
                for fit in dance.Fits do
                    entryCount team.Dances fit
            } |> Seq.sum
        count + fitCount

    //we need to continue using this rather than use the Team as the key
    //is because the order of person1 and 2 is not always consistent
    let personTeamKey person1 person2 =
        let names = [person1.Name; person2.Name]
        let sortedNames = List.sort names
        $"{sortedNames.[0]}_{sortedNames.[1]}"

    let personTeamKey2 team = personTeamKey team.Person1 team.Person2

    let getDanceSet allTeams =
        allTeams 
        |> Seq.collect (fun t -> t.Dances |> Seq.map (fun kvp -> kvp.Key))
        |> Set.ofSeq