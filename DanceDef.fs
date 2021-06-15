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
        }


    type RankDictionary =
        {
            AgeGroups: Map<string, int>
            SkillLevels: Map<string, int>
            MultiDanceAgeGroup: Map<string, int>
            MultiDanceSkillLevels: Map<string, int>
        }
