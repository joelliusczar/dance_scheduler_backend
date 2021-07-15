module Score
  open Person
  open Dance
  open Teams
  open Studio
  open DanceDef
  open SoloDance

  let getCatagory person1 person2 =
    match person1.Role.Name with
    | "LEAD" -> 
      match person1.Qualification.Name with
      | "PRO" -> 
        match person2.Qualification.Name with 
        | "PRO_DEV" -> "L-PD"
        | "AM" -> "L"
        | _ -> "UNKNOWN PRO FULL FOLLOW"
      | "PRO_DEV" -> 
        match person2.Qualification.Name with 
        | "PRO" -> "G-PD"
        | "PRO_DEV" -> "TBD BOTH DEV"
        | "AM" -> "L"
        | _ -> "UNKNOWN PRO DEV FOLLOW"
      | "AM" -> 
        match person2.Qualification.Name with 
        | "PRO" -> "G"
        | "PRO_DEV" -> "G"
        | "AM" -> "AC-G"
        | _ -> "UNKNOWN AM FOLLOW"
      | _ -> "UNKNOWN QUALIFICATION LEAD"
    | "FOL" -> 
      match person1.Qualification.Name with
      | "PRO" -> 
        match person2.Qualification.Name with 
        | "PRO" -> "TBD BOTH FULL PRO"
        | "PRO_DEV" -> "G-PD"
        | "AM" -> "G"
        | _ -> "UNKNOWN PRO FULL LEADER"
      | "PRO_DEV" -> 
        match person2.Qualification.Name with 
        | "PRO" -> "L-PD"
        | "PRO_DEV" -> "TBD BOTH DEV"
        | "AM" -> "G"
        | _ -> "UNKNOWN PRO FULL LEADER"
      | "AM" -> 
        match person2.Qualification.Name with 
        | "PRO" -> "L"
        | "PRO_DEV" -> "L"
        | "AM" -> "AC"
        | _ -> "UNKNOWN PRO FULL LEADER"
      | _ -> "UNKNOWN QUALIFICATION FOLLOW"
    | _ -> "UNKNOWN ROLE"

  let getTeamType person1 person2 =
    match person1.Qualification.Name with
    | "AM" -> 
      match person2.Qualification.Name with 
      | "AM" -> "AM/AM"
      | prefix when prefix.StartsWith("PRO") -> "PRO/AM"
      | _ -> "UNKNOWN TYPE"
    | prefix when prefix.StartsWith("PRO") -> 
      match person2.Qualification.Name with 
      | "AM" -> "PRO/AM"
      | "PRO_FULL" -> "PRO/PRO"
      | "PRO_DEV" -> "PRO/AM"
      | _ -> "UNKNOWN TYPE"
    | _ -> "UNKNOWN TYPE"

  let skilllevelMajorMapToCode skillLevel = 
    match skillLevel.MajorVersion.Name with
    | "Bronze" -> "BRO"
    | "Silver" -> "SIL"
    | "Gold" -> "GOLD"
    | _ -> "UKN"

  let skilllevelMinorMapToCode skillLevel = 
    match skillLevel.MinorVersion.Name with
    | "Newcomer" -> "NEW"
    | "Intermediate" -> "INT"
    | "Full" -> "FULL"
    | "Open" -> "OPEN"
    | "Multi 3" -> "GOLD"
    | "Multi 4" -> "GOLD"
    | "Open 5" -> "GOLD"
    | _ -> "UKN"

  type ScoredDance = 
    {
      Name: string
      FullName: string
      Order: int
      PreHeatNumber: int
    }

  type Score = 
    {
      Studio: Studio
      TeamNumber: int
      LeaderNumber: int
      Person1: Person
      Person2: Person
      RoundNumber: int
      ScoredDance: ScoredDance  
      AgeGroup: DivisionedGroup
      SkillLevel: DivisionedGroup
      Plurality: BaseDef
    }

    member this.Catagory() = 
      getCatagory this.Person1 this.Person2

    member this.AgeGroupStr() = 
      $"{this.AgeGroup.MajorVersion.Name}{this.AgeGroup.MinorVersion.Name}"

    member this.SkillLevelStr() =
      $"{this.SkillLevel.MajorVersion.Name}{this.SkillLevel.MinorVersion.Name}"

    member this.TeamType() =
      getTeamType this.Person1 this.Person2

    member this.Level1() =
      skilllevelMajorMapToCode this.SkillLevel

    member this.Level2() =
      skilllevelMinorMapToCode this.SkillLevel

    member this.PreRoundNumber() = 
      if this.Plurality.Name = "Multi"
      then $"{this.RoundNumber}.{this.ScoredDance.PreHeatNumber + 1}"
      else $"{this.RoundNumber}"

  let teamEntryToScoreEntry entry (person1: Person) person2 (childDanceDef: DanceDef) idx =
    {
        Studio = person1.Studio
        TeamNumber = entry.Team.TeamNumber
        LeaderNumber = entry.Team.LeaderNumber 
        Person1 = person1
        Person2 = person2
        RoundNumber = entry.RoundNumber + 1
        ScoredDance = {
          Name = childDanceDef.Name
          FullName = childDanceDef.FullName
          Order = childDanceDef.Order
          PreHeatNumber = idx
        }
        AgeGroup = entry.Dance.AgeGroup
        SkillLevel = entry.Dance.SkillLevel
        Plurality = entry.Dance.DanceDef.Plurality
    }
  
  let soloDanceToScoreEntry entry = 
    {
        Studio = entry.TeamDef.Person1.Studio
        TeamNumber = entry.TeamDef.TeamNumber
        LeaderNumber = entry.TeamDef.LeaderNumber 
        Person1 = entry.TeamDef.Person1
        Person2 = entry.TeamDef.Person2
        RoundNumber = entry.RoundNumber
        ScoredDance = {
          Name = entry.Name
          FullName = entry.FullName
          Order = 0
          PreHeatNumber = 0
        }
        AgeGroup = entry.AgeGroup
        SkillLevel = entry.SkillLevel
        Plurality = {
          Name = "SO"
          FullName = "Solo"
          Order = 0
        }
    }

  let teamEntryToMultiScores compDefs entry person1 person2 =
    seq {
      match entry.Dance.DanceDef.Plurality.Name with
      | "Multi" -> 
        yield! entry.Dance.DanceDef.ComponentDanceKeys 
        |> Seq.map (fun k -> compDefs.DanceDefs.[k]) 
        |> Seq.mapi (fun idx d -> 
          teamEntryToScoreEntry entry person1 person2 d idx
        )
      | _ -> yield teamEntryToScoreEntry entry person1 person2 entry.Dance.DanceDef 0
    }

  let entryToScores compDefs danceEntry =
    seq {
      match danceEntry with
      | TeamEntry entry -> 
        let person1 = entry.Team.Person1
        let person2 = entry.Team.Person2
        yield! teamEntryToMultiScores compDefs entry person1 person2
        let p1Qualification = person1.Qualification.Name 
        let p2Qualification = person2.Qualification.Name 
        if p1Qualification = "AM" && p2Qualification = "AM" 
        then yield! teamEntryToMultiScores compDefs entry person2 person1
      | SoloDance entry -> yield soloDanceToScoreEntry entry
    }