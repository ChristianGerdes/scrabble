// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

[<StructuredFormatDisplay("{AsString}")>]
type MultiSet<'a when 'a: comparison> =
    | M of Map<'a, uint32>
    override this.ToString() =
        match this with
        | M m ->
            let str =
                Map.fold
                    (fun acc key value ->
                        acc
                        + "("
                        + key.ToString()
                        + ", #"
                        + value.ToString()
                        + "), ")
                    ""
                    m

            str.Remove(str.Length - 2, 2) |> sprintf "{%s}"

    member this.AsString = this.ToString()

let empty = M(Map.empty)

let isEmpty (M m) = m.IsEmpty

let size (M m) =
    Map.fold (fun acc _ value -> acc + value) 0u m

let contains (a) (M m) = m.ContainsKey(a)

let numItems (a) (M m) =
    if (m.IsEmpty || not (m.ContainsKey(a))) then
        0u
    else
        m.Item(a)

let add (a) (n) (M m) =
    if m.ContainsKey(a) then
        M(m.Add(a, n + (numItems a (M m))))
    else
        M(m.Add(a, n))

let addSingle (a) (M m) =
    if m.ContainsKey(a) then
        M(m.Add(a, 1u + (numItems a (M m))))
    else
        M(m.Add(a, 1u))

let remove (a) (n) (M m) =
    if m.Item(a) > n then
        M(m.Add(a, m.Item(a) - n))
    else
        M(m.Remove(a))

let removeSingle (a) (M m) =
    if m.ContainsKey(a) then
        (remove a 1u (M m))
    else
        M m

let fold f acc (M m) =
    Map.fold (fun acc key value -> (f acc key value)) acc m

let foldBack f (M m) acc =
    Map.foldBack (fun key value acc -> (f key value acc)) m acc

let map f m =
    fold (fun acc key value -> add (f key) value acc) empty m

let ofList l =
    List.fold (fun acc key -> addSingle key acc) empty l

let toList (M m) =
    Map.toList (m)
    |> List.collect (fun (key, value) -> List.replicate (int (value)) key)

let union s1 s2 =
    fold
        (fun acc key value ->
            if (contains key acc) then
                if (value > numItems key acc) then
                    add key (numItems key s2) acc
                else
                    acc
            else
                add key (numItems key s2) acc)
        s1
        s2

let sum s1 s2 =
    fold
        (fun acc key value ->
            if (contains key acc) then
                add key (numItems key s2) acc
            else
                add key (numItems key s1) acc)
        s1
        s2

let subtract s1 s2 =
    fold
        (fun acc key value ->
            if (contains key acc) then
                remove key (numItems key s2) acc
            else
                acc)
        s1
        s2

let intersection s1 s2 =
    fold
        (fun acc key value ->
            if (contains key s2) then
                if (value > numItems key s1) then
                    add key value acc
                else
                    add key (numItems key s2) acc
            else
                acc)
        empty
        s1
