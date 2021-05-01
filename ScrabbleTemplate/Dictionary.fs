// Insert your Dictionary.fsi file here. All modules must be internal.
module internal Dictionary
    open System.Collections.Generic

    type Dict = | D of Dictionary<char, bool * Dict>

    // If you have made a lookup function you may include it here, but it will be generated for you by the project.

    let empty : unit -> Dict = fun () -> D (new Dictionary<char, bool * Dict>())
    
    let step (c: char) (D dict) = 
        match dict.ContainsKey c with
        | true -> Some dict.[c]
        | false -> None

    let unpack (D d) = d
    // let insert : string -> Dict -> Dict = fun s (D d) -> D (d.Add(s))
    let insert (s: string) (D dict) = 
        let rec go cl (dict : Dictionary<char, bool * Dict>) = 
            match cl with
            | [] -> ()
            | head::tail when tail.Length = 0 -> 
                match (step head (D dict)) with
                | Some c -> dict.[head] <- (true, snd c)
                | None -> dict.[head] <- (true, empty ())
            | head::tail -> 
                match (step head (D dict)) with
                | Some c -> go tail (unpack (snd c))
                | None -> dict.[head] <- (false, empty ())
                          go tail (unpack (snd dict.[head]))
        go (List.ofSeq s) dict
        D dict

    let trie = empty ()
    insert "book" trie
    insert "boo" trie
    insert "bool" trie
    insert "booligan" trie

    let lookup (s: string) (D dict) =
        let rec go cl dict =
            match cl with
            | [] -> false
            | head::tail when tail.Length = 0 ->
                match (step head (D dict)) with
                | Some c -> fst dict.[head]
                | None -> false
            | head::tail ->
                match (step head (D dict)) with
                | Some c -> go tail (unpack (snd c))
                | None -> false
        go (List.ofSeq s) dict
        
    lookup "book" trie
    lookup "hejsa" trie
    
    // Only implement reverse if you have made a Gaddag
    let reverse : Dict -> (bool * Dict) option = fun _ -> failwith "Not implemented"

    // let lookup : string -> Dict -> bool = fun _ _ -> failwith "Not implemented"

