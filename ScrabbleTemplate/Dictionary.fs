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

    // type Dict = | D of Map<char, bool * Dict>

    // // If you have made a lookup function you may include it here, but it will be generated for you by the project.

    // let empty : unit -> Dict = fun () -> D (Map.empty)
    
    // let step (c: char) (D dict) = 
    //     match dict.ContainsKey c with
    //     | true -> Some dict.[c]
    //     | false -> None
        
    // // let insert : string -> Dict -> Dict = fun s (D d) -> D (d.Add(s))
    // let insert (s: string) (D dict) = 
    //     let rec go cl (dict : Map<char, bool * Dict>) = 
    //         match cl, dict with
    //         | ([],_) -> empty()
    //         | head::tail, _ when tail.Length = 0 -> 
    //             match (step head (D dict)) with
    //             | Some c -> D (Map.add head (true, snd c) dict)
    //             | None -> D (Map.add head (true, empty ()) dict)
    //         | head::tail, _ -> 
    //             match (step head (D dict)), dict with
    //             | Some c, d -> D (Map.add head (false, go tail (snd c)) d)
    //             // | Some c, d -> D (Map.add head (false, snd c) d)
    //             | None, d -> D (Map.add head (false, empty ()) d)    
    //     go (List.ofSeq s) dict

    
    

// (C , (false, empty))

// dict = []
   
// CAR 
// C

    // Only implement reverse if you have made a Gaddag
    let reverse : Dict -> (bool * Dict) option = fun _ -> failwith "Not implemented"




// module Dict
//    type Dict= | D of Set<string>

//    let empty = fun () -> D (Set.empty)

//    let insert s (D d) =  D (d.Add(s))

//    let lookup s (D d) =  d.Contains(s)


