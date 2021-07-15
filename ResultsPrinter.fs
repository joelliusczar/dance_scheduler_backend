module ResultsPrinter
  open Round
  open Teams
  open Dance
  open Person
  open DanceDef
  open Score
  open SoloDance

  let getSkillLevelStr (dance: Dance) =
    let skillLevel1 = dance.SkillLevel.MajorVersion.FullName
    let skillLevel2Raw = dance.SkillLevel.MinorVersion.FullName
    let skillLevel2 = if skillLevel2Raw = "--" then "" else skillLevel2Raw
    $"{skillLevel1} {skillLevel2}"

  let strFormatRound _ (round: Round) number =
    let header1 = $"Dance {round.DanceDef.FullName} Heat# {round.Number + 1}\n"
    let header2 = $"Leader | Follower | Age Group | Skill Level\n"
    let entries = 
        round.TeamEntries
        |> Seq.map (fun e -> 
            let skillLevel = getSkillLevelStr e.Dance
            $"{e.Team.Person1.Name} | {e.Team.Person2.Name} | {e.Dance.AgeGroup} | {skillLevel}"
        )
        |> String.concat "\n"
    $"{header1}{header2}{entries}\n"

  let strFormatAllRounds compDefs rowFormatter allRounds =
    allRounds
    |> Seq.map (fun idx r -> rowFormatter compDefs r idx)

  let formatCsvRound _ round number =
      //let header = $"Leader,Follower,Dance,AgeGroup,SkillLevel,HeatNumber\n"
    let entries = 
      round.TeamEntries
      |> Seq.map (fun e -> 
        let skillLevel = getSkillLevelStr e.Dance
        $"\"{e.Team.Person1.Name}\",\"{e.Team.Person2.Name}\",\"{e.Dance.DanceDef.FullName}\",\"{e.Dance.AgeGroup}\",\"{skillLevel}\",\"{number}\""
      )
      |> String.concat "\n"
    entries

  let scoreHeader =
    "STUDIO,
    COMP #,
    Day,
    PRE-HEAT,
    HEAT,
    DAN,
    CAT,
    AGE,
    LDR #,
    NAME,
    PARTNER,
    TYPE,
    AGE LEV,
    AGE GRP,
    H-TYPE,
    HEAT ASSIGN,
    LEV 1,
    LEV 2,
    Dan Sort,
    DIV,
    TD,
    DS,
    TOTAL,
    # of Scores,
    Average,
    LEV1 SORT,
    LEV2 SORT,
    CAT SORT,
    AGE SORT,
    SORT CRITERIA,
    Full Level Name,
    DANCE,
    Solo Dances".Replace("\n","")

  let formatScoreEntry score idx =
    $"{score.Studio.Number},
      {score.TeamNumber},
      Day Placeholder,
      {score.PreRoundNumber()},
      {score.RoundNumber},
      {score.ScoredDance.Name},
      {score.Catagory()},
      {score.AgeGroupStr()},
      {score.LeaderNumber},
      {score.Person1.Name},
      {score.Person2.Name},
      {score.TeamType()},
      {score.AgeGroup.MajorVersion.Name},
      {score.AgeGroup.MinorVersion.Name},
      {score.Plurality.Name},
      {score.ScoredDance.FullName},
      {score.SkillLevelStr()}".Replace("\n","")

  let formatScoreRound compDefs round number =
    
    let entries = 
      round.TeamEntries
      |> Seq.map (fun e -> 
        entryToScores compDefs (DanceEntry.TeamEntry e)
      )
      |> Seq.concat
      |> Seq.map (fun e -> 
        formatScoreEntry e number
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
            let skillLevel = getSkillLevelStr e.Dance
            $"Heat #{e.RoundNumber} {e.Dance.DanceDef.FullName} {e.Dance.AgeGroup} | {skillLevel} | {teamMateName}"
        )
        |> String.concat "\n"
    $"${lines}"

        // let strFormatAllRoundSheets personRoundMap =
        //     personRoundMap
        //     |> Seq.map (fun kvp -> strFormatPersonRoundSheet kvp)