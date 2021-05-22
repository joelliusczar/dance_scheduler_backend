module DanceDef

    type DanceDef = 
        {
            Name: string
            FullName: string
            Style: string
            Plurality: string
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
