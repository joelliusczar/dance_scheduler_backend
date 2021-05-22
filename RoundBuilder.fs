module RoundBuilder
    open DanceDef
    open Teams
    open Round


    let eligibleTeams teams dance = 
        teams 
        |> Seq.filter (fun p -> getDanceCount p dance > 0)
        |> Seq.toList

    let markAllRecentTeams allTeams usedTeams =
        match usedTeams with
        | h::t ->
            let lastRoundNumber = h.LastRoundNumber
            let usedPeople =
                usedTeams
                |> Seq.collect (fun t -> [t.TeamDef.Person1; t.TeamDef.Person2])
                |> Set.ofSeq
            allTeams 
            |> List.map (fun t ->
                let personMatch1 = Set.contains t.TeamDef.Person1 usedPeople 
                let personMatch2 = Set.contains t.TeamDef.Person2 usedPeople 
                match personMatch1 || personMatch2 with
                | false -> t
                | true -> { t with LastRoundNumber = lastRoundNumber}
            )
        | _ -> []

    let rankEligible rankDefs teams round =
        let recentTeams = 
            teams
            |> List.groupBy (fun t -> t.LastRoundNumber)
            |> List.sortByDescending (fun (k,g) -> k)
            |> List.map (fun (k, g) -> g)
            |> List.tryHead
        match recentTeams with
        | None -> []
        | Some recentTeams ->
            let markedTeams = markAllRecentTeams teams recentTeams
            markedTeams
            |> List.sortByDescending (fun t ->
                round.Number - t.LastRoundNumber + 1
            )

    let getRemainingTeams allTeams dances =
        allTeams
        |> Seq.filter (fun t ->
            dances  
            |> Seq.map (fun d -> getDanceCount t d)
            |> Seq.exists (fun n -> n > 0) 
        )
        

    let rec buildRound dances allTeams eligible round =
        let (team, eligible2) = 
            match eligible with
            | h::t when List.length round.TeamEntries < 6 -> (Some h,t)
            | _ -> (None, [])
        match team with
        | None -> (round, allTeams)
        | Some team -> 
            match tryConsumeDance team round with
            | None, _ -> 
                match getDanceCount team round.DanceDef < 1 with
                | false -> buildRound dances allTeams eligible2 round
                | true -> 
                    let remainingTeams =  allTeams |> Seq.filter (fun c -> 
                        c.TeamDef <> team.TeamDef)
                    buildRound dances remainingTeams eligible2 round
            | Some dance, team2 ->
                let round2 = updateRoundWithTeam round team2.TeamDef dance
                let replacedTeams = allTeams |> Seq.map (fun t ->
                    match t.TeamDef = team2.TeamDef with
                    | false -> t
                    | true -> team2
                    )
                buildRound dances replacedTeams eligible2 round2
                        

    let rec buildRoundsForStyle 
        rankDefs 
        (relevantDances: list<_>) 
        allTeams 
        danceIdx 
        rounds =
        match List.isEmpty relevantDances || Seq.isEmpty allTeams with
        | true -> rounds
        | false ->
            let danceDef = relevantDances.[danceIdx % relevantDances.Length]
            let remainingTeams = getRemainingTeams allTeams relevantDances
            let eligible = eligibleTeams remainingTeams danceDef
            let round = { 
                Number = danceIdx
                DanceDef = danceDef
                TeamEntries = List.empty
                Impossible = Set.empty
            }
            let rankedEligible = rankEligible rankDefs eligible round
            let (round2, remainingTeamsAfterRound) = 
                buildRound 
                    relevantDances 
                    remainingTeams 
                    rankedEligible 
                    round
            let buildRemainingRounds = buildRoundsForStyle rankDefs relevantDances
            buildRemainingRounds 
                remainingTeamsAfterRound 
                (danceIdx + 1) 
                (round2::rounds)

    let filterDanceSetForTeams allTeams relevantDancesKeys =
        allTeams
        |> List.map (fun t -> 
            { 
                t with 
                    Dances = t.Dances 
                    |> Map.filter (fun k v -> 
                        Set.contains k relevantDancesKeys 
                    )
            }
        )

    let buildComp  
        (danceLookup: Map<string, DanceDef>) 
        rankDefs 
        allTeams =
        let presentDanceSet = getDanceSet allTeams
        let styles = 
            danceLookup 
            |> Seq.map (fun kvp -> kvp.Value)
            |> Seq.map (fun v -> v.Style)
            |> Seq.distinct
        let pluralities = 
            danceLookup
            |> Seq.map (fun kvp -> kvp.Value)
            |> Seq.map (fun v -> v.Plurality)
            |> Seq.distinct

        Seq.allPairs styles pluralities
        |> Seq.map (fun (style, plurality) ->
            let relevantDances = 
                danceLookup 
                |> Seq.map (fun kvp -> kvp.Value)
                |> Seq.filter (fun v -> v.Plurality = plurality)
                |> Seq.filter (fun v -> v.Style = style)
                |> Seq.filter (fun v -> Set.contains v.Name presentDanceSet)
                |> Seq.sortBy (fun v -> v.Order)
                |> Seq.toList
            let relevantDancesKeys = 
                relevantDances 
                |> Seq.map (fun d -> d.Name)
                |> Set.ofSeq
            let danceSetTeams = filterDanceSetForTeams allTeams relevantDancesKeys
            buildRoundsForStyle rankDefs relevantDances danceSetTeams 0 List.empty 
            |> List.rev
        )