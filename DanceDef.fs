module DanceDef

    type BaseDef =
        {
            Name: string
            FullName: string
            Order: int
        }

    type DanceDef = 
        {
            Name: string
            FullName: string
            Style: BaseDef
            Plurality: BaseDef
            Order: int
            Fits: string list
            ComponentDanceKeys: string list
        }


    type RankDictionary =
        {
            AgeGroups: Map<string, BaseDef>
            SkillLevels1: Map<string, BaseDef>
            SkillLevels2: Map<string, BaseDef>
            QualificationLevels: Map<string, int>
            Roles: Map<string, int>
        }

    type CompetitionDefinitions =
        {
            DanceDefs: Map<string, DanceDef>
            RankDefs: RankDictionary
            IgnoredKeys: Set<string>
        }