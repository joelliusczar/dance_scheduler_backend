module Person
  open Studio

    type DanceRole = 
      {
        Name: string
      }

    type QualificationRole = 
      {
        Name: string
      }

    type Person = 
        {
            Name: string
            Role: DanceRole
            Qualification: QualificationRole
            Studio: Studio
        }
