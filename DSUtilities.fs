module DSUtilities
    let findOrDefaultList (map:Map<'a, List<_>>) key =
        match map.TryFind(key) with
        | Some findResult -> findResult
        | None -> []

    let entryCount map key =
        findOrDefaultList map key |> List.length

    let rec removeN n filter list=
        match list with
            | h::t when filter h && n > 0 -> removeN (n - 1) filter t
            | h::t -> h::removeN n filter t
            | _ -> []