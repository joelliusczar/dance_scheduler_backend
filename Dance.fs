
module Dance
    open DanceDef
    

    type DivisionedGroup =
        {
            MajorVersion: BaseDef
            MinorVersion: BaseDef
        }

        override this.ToString() = $"{this.MajorVersion.Name}{this.MinorVersion.Name}"

    type Dance =
        {
            DanceDef: DanceDef
            AgeGroup: DivisionedGroup
            SkillLevel: DivisionedGroup
        }

