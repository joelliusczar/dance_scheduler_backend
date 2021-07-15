module RoundTeams
  open Round
  open Teams

  let tryConsumeDance team round =
    let blockPerson1 = round.Impossible.Contains team.TeamDef.Person1.Name
    let blockPerson2 = round.Impossible.Contains team.TeamDef.Person2.Name
    let blockTeam = blockPerson1 || blockPerson2
    if blockTeam
    then (None, team)
    else
        let dance, team2 = consumeDance team round.DanceDef
        (dance, { team2 with LastRoundNumber = round.Number + 1 })

  let updateRoundWithTeam round team dance=
    let teamEntry = {
        Team = team
        Dance = dance
        RoundNumber = round.Number
    }
    {
        round with
            Impossible = round.Impossible 
            |> Set.add team.Person1.Name 
            |> Set.add team.Person2.Name
            TeamEntries = teamEntry :: round.TeamEntries
    }