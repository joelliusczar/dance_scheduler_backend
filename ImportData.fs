module ImportData
    open System
    open System.IO
    open DanceDef
    open Dance
    open Person
    open Teams
    open SoloDance

    //entries columns
    let PERSON1_NAME_COL = 0
    let PERSON1_QUALIFICATION = 1
    let PERSON1_ROLE = 2
    let PERSON2_NAME_COL = 3
    let PERSON2_QUALIFICATION = 4
    let PERSON2_ROLE = 5
    let TEAM_NUM_COL = 6
    let LEADER_NUM_COL = 7
    let DANCE_COL = 8
    let AGE_GROUP_MAJOR_COL = 9
    let AGE_GROUP_MINOR_COL = 10
    let SKILL_LVL_MAJOR_COL = 11
    let SKILL_LVL_MINOR_COL = 12

    //definitions columns
    let DANCE_CODE_COL = 0
    let DANCE_FULL_NAME_COL = 1
    let RELATED_DANCES_COL = 2
    let COMPONENT_DANCES_COL = 3
    let STYLE_COL = 4
    let PLURALITY_COL = 5
    let AGE_GROUP_DEF_COL = 6
    let QUALIFICATION_COL = 7
    let SKILL_LEVEL_DEF_MAJOR_COL = 8
    let SKILL_LEVEL_DEF_MINOR_COL = 9
    let STYLE_DISCREET_COL = 10
    let PLURALITY_DISCREET_COL = 11
    let DANCE_ROLE_COL = 12
    let IGNORED_DANCES_COL = 13

    

    let personCollect (values: string[]) colKey (people: Map<string, Person>) = 
        if people.ContainsKey (values.[colKey].Trim())
            then (people.[values.[colKey].Trim()], people)
            else
                let p = { 
                    Name = values.[colKey].Trim()
                    Qualification = { Name = values.[colKey + 1].Trim() }
                    Role = { Name = values.[colKey + 2].Trim() }
                    Studio = { Name = ""; Number = 0; }
                }
                let updated = people.Add(values.[colKey].Trim(), p)
                (p, updated)


    let rec buildDataFromStream 
        (reader: StreamReader) 
        compDefs
        (people: Map<string, Person>) 
        (teams: Map<string, Team>) 
        (solos: List<SoloDance>) =
        match reader.ReadLine() with
        | null -> 
            (Some people, Some teams, Some solos)
        | line -> 
            let values = line.Split([|','|])
            let danceKey = values.[DANCE_COL].Trim().Replace("-","_")
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
                        TeamNumber = values.[TEAM_NUM_COL].Trim() |> int
                        LeaderNumber = values.[LEADER_NUM_COL].Trim() |> int
                    }
                    Dances = Map.empty
                    LastRoundNumber = 0
                }
            let skillLevel1 = values.[SKILL_LVL_MAJOR_COL].Trim()
            let skillLevel2Raw = values.[SKILL_LVL_MINOR_COL].Trim()
            let skillLevel2 = if String.IsNullOrEmpty skillLevel2Raw then "--" else skillLevel2Raw
            let ageGroup1Raw = values.[AGE_GROUP_MAJOR_COL].Trim()
            let ageGroup2Raw = values.[AGE_GROUP_MINOR_COL].Trim()
            let ageGroup1 = if String.IsNullOrEmpty ageGroup1Raw then "--" else ageGroup1Raw
            let ageGroup2 = if String.IsNullOrEmpty ageGroup2Raw then "--" else ageGroup2Raw
            printfn $"{skillLevel1}"
            printfn $"{skillLevel2}"
            printfn $"{ageGroup1}"
            if Map.containsKey danceKey compDefs.DanceDefs 
            then
                let ageVersionMajor = compDefs.RankDefs.AgeGroups.[ageGroup1]
                let skillVersionMajor = compDefs.RankDefs.SkillLevels1.[skillLevel1]
                let skillVersionMinor = compDefs.RankDefs.SkillLevels2.[skillLevel2]
                let dance = { 
                    DanceDef = compDefs.DanceDefs.[danceKey]
                    AgeGroup = {
                        MajorVersion = ageVersionMajor
                        MinorVersion = {
                            Name = ageGroup2
                            FullName = ageGroup2
                            Order = 0 //TODO fix ageGroup2 |> int
                        }
                    } 
                    SkillLevel = {
                        MajorVersion = skillVersionMajor
                        MinorVersion = skillVersionMinor
                    } 
                }
                let team2 = updateTeamWithDance team dance
                let teams2 = teams |> update (personTeamKey2 team2.TeamDef) team2
                buildDataFromStream reader compDefs people3 teams2 solos
            else
                let dance = {
                    Name = danceKey
                    FullName = danceKey
                    AgeGroup = {
                        MajorVersion = compDefs.RankDefs.AgeGroups.[ageGroup1]
                        MinorVersion = {
                            Name = ageGroup2
                            FullName = ageGroup2
                            Order = 0//ageGroup2 |> int //TODO fix ageGroup2 |> int
                        }
                    } 
                    RoundNumber = 0 //TODO: #SOLO_ROUND_NUM 
                    SkillLevel = {
                        MajorVersion = compDefs.RankDefs.SkillLevels1.[skillLevel1]
                        MinorVersion = compDefs.RankDefs.SkillLevels2.[skillLevel2]
                    }
                    TeamDef = team.TeamDef
                }
                buildDataFromStream reader compDefs people teams (dance :: solos)


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

    let keyRankToKeyDef (key,rank) =
        (key, {
            Name = key
            FullName = key
            Order = rank
        })


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

    let buildKeyList cellValue =
        cellValue
        |> (fun r -> 
            if String.IsNullOrWhiteSpace r
            then []
            else
                r.Split([|'|'|]) 
                |> Array.toList 
                |> List.map (fun c -> c.Trim())

        )
    


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
                        Fits = buildKeyList allLines.[idx].[RELATED_DANCES_COL]
                        ComponentDanceKeys = buildKeyList allLines.[idx].[COMPONENT_DANCES_COL]
                    }
                    yield (name,def)
            } |> Map.ofSeq
        let colToSeq2 = colToSeq allLines
        let rankDefs = {
            AgeGroups = 
                colToSeq2 AGE_GROUP_DEF_COL 
                |> List.ofSeq
                |> positionRankValues 
                |> Seq.map keyRankToKeyDef
                |> Map.ofSeq
            SkillLevels1 = 
                colToSeq2 SKILL_LEVEL_DEF_MAJOR_COL 
                |> List.ofSeq
                |> positionRankValues 
                |> Seq.map keyRankToKeyDef
                |> Map.ofSeq
            SkillLevels2 = 
                colToSeq2 SKILL_LEVEL_DEF_MINOR_COL 
                |> List.ofSeq
                |> positionRankValues 
                |> Seq.map keyRankToKeyDef
                |> Map.ofSeq
            QualificationLevels = 
                colToSeq2 QUALIFICATION_COL 
                |> List.ofSeq
                |> positionRankValues 
                |> Map.ofSeq
            Roles = 
                colToSeq2 DANCE_ROLE_COL 
                |> List.ofSeq
                |> positionRankValues 
                |> Map.ofSeq
        }
        {
            DanceDefs = danceDefs
            RankDefs = rankDefs
            IgnoredKeys = Set.empty
        }
        
