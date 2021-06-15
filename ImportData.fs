module ImportData
    open System
    open System.IO
    open DanceDef
    open Dance
    open Teams

    //entries columns
    let PERSON1_NAME_COL = 0
    let PERSON2_NAME_COL = 1
    let TEAM_NUM_COL = 2
    let DANCE_COL = 3
    let AGE_GROUP_COL = 4
    let AGE_GROUP_MOD_COL = 5
    let SKILL_LVL_COL = 6
    let GROUP_COL = 7

    //definitions columns
    let DANCE_CODE_COL = 0
    let DANCE_FULL_NAME_COL = 1
    let RELATED_DANCES_COL = 2
    let STYLE_COL = 3
    let PLURALITY_COL = 4
    let AGE_GROUP_DEF_COL = 5
    let AGE_GROUP_DEF_MOD_COL = 6
    let SKILL_LEVEL_DEF_COL = 7
    let MULTI_AGE_GROUP_COL = 8
    let MULTI_SKILL_LEVEL_COL = 9
    let STYLE_DISCREET_COL = 10
    let PLURALITY_DISCREET_COL = 11

    let personCollect (values: string[]) colKey (people: Map<string, Person>) = 
        match people.ContainsKey (values.[colKey].Trim()) with
            | true -> (people.[values.[colKey].Trim()], people)
            | false ->
                let p = { Name = values.[colKey].Trim() }
                let updated = people.Add(values.[colKey].Trim(), p)
                (p, updated)

    let rec buildDataFromStream 
        (reader: StreamReader) 
        (danceLookup: Map<string, DanceDef>)
        (people: Map<string, Person>) 
        (teams: Map<string, Team>) =
        match reader.ReadLine() with
        | null -> 
            (people, teams)
        | line -> 
            let values = line.Split([|','|])
            let person1, people2 = personCollect values PERSON1_NAME_COL people
            let person2, people3 = personCollect values PERSON2_NAME_COL people
            let teamKey = personTeamKey person1 person2
            let team = 
                match teams.TryFind(teamKey) with
                | Some findResult -> findResult
                | None -> {
                    TeamDef = {
                        Person1 = person1
                        Person2 = person2
                    }
                    TeamNumber = values.[TEAM_NUM_COL] |> int
                    Dances = Map.empty
                    LastRoundNumber = 0
                }
            let dance = { 
                DanceDef = danceLookup.[values.[DANCE_COL].Trim()]
                AgeGroup = values.[AGE_GROUP_COL].Trim()
                SkillLevl = values.[SKILL_LVL_COL].Trim()
            }
            let team2 = updateTeamWithDance team dance
            let teams2 = teams |> update (personTeamKey2 team2.TeamDef) team2
            buildDataFromStream reader danceLookup people3 teams2

    let colToSeq (values: string array array) col =
        seq {
            for idx in 0..values.Length - 1 do
                let cellValue = values.[idx].[col].Trim()
                if not (String.IsNullOrEmpty cellValue) then
                    yield cellValue
        }

    let positionRankValues (values: List<_>) =
        let calcWeight count idx =
            let k = count / 2
            match k, idx, count with
            | k, idx, count when idx >= k && (count % 2) = 0 -> idx - k + 1
            | k, idx, _ -> idx - k
        let rec _positionRankValues _values idx =
            match _values with
                | [] -> []
                | h::t-> 
                    let weight = calcWeight ((List.length _values) + idx) idx
                    (h,weight)::_positionRankValues t (idx + 1)
        _positionRankValues values 0


    let toBaseDefsDict (values: string array array) col =
        seq {
            for idx in 0..values.Length - 1 do
                let name = values.[idx].[col].Trim()
                let def = {
                    Name = name
                    FullName = name
                    Order = idx
                }
                yield (name, def)
        } |> Map.ofSeq




    let buildDefsFromStream (reader: StreamReader) =
        let allLines = 
            reader.ReadToEnd().Split([|'\n'|])
            |> Array.map (fun l -> l.Split([|','|]))
        let stylesMap = toBaseDefsDict allLines STYLE_DISCREET_COL
        let pluralityMap = toBaseDefsDict allLines PLURALITY_DISCREET_COL
        let danceDefs = 
            seq {
                for idx in 0..allLines.Length - 1 do
                    let name = allLines.[idx].[DANCE_CODE_COL].Trim()
                    let styleKey = allLines.[idx].[STYLE_COL].Trim()
                    let pluralityKey = allLines.[idx].[PLURALITY_COL].Trim()
                    let def = {
                        Name = name
                        FullName = allLines.[idx].[DANCE_FULL_NAME_COL].Trim()
                        Style = stylesMap.[styleKey]
                        Plurality = pluralityMap.[pluralityKey]
                        Order = idx
                        Fits = 
                            allLines.[idx].[RELATED_DANCES_COL]
                            |> (fun r -> 
                                match String.IsNullOrWhiteSpace r with
                                | true -> []
                                | false -> 
                                    r.Split([|'|'|]) 
                                    |> Array.toList 
                                    |> List.map (fun c -> c.Trim())

                            )
                    }
                    yield (name,def)
            } |> Map.ofSeq
        let colToSeq2 = colToSeq allLines
        let rankDefs = {
            AgeGroups = 
                colToSeq2 AGE_GROUP_DEF_COL 
                |> List.ofSeq
                |> positionRankValues 
                |> Map.ofSeq
            SkillLevels = 
                colToSeq2 SKILL_LEVEL_DEF_COL 
                |> List.ofSeq
                |> positionRankValues 
                |> Map.ofSeq
            MultiDanceAgeGroup = 
                colToSeq2 MULTI_AGE_GROUP_COL 
                |> List.ofSeq
                |> positionRankValues 
                |> Map.ofSeq
            MultiDanceSkillLevels = 
                colToSeq2 MULTI_SKILL_LEVEL_COL 
                |> List.ofSeq
                |> positionRankValues 
                |> Map.ofSeq
        }
        (danceDefs, rankDefs)
        
