module SoloDance
  open Dance
  open DanceDef
  open Teams

  type SoloDance = 
    {
        Name: string
        FullName: string
        AgeGroup: DivisionedGroup
        SkillLevel: DivisionedGroup
        TeamDef: TeamDef
        RoundNumber: int
    }

  type DanceEntry = SoloDance of SoloDance | TeamEntry of TeamEntry