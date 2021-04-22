type Trie =
    | Leaf of char
    | Node of char * bool * Trie list


let rec addLeaves (word: char list) =
    match List.length word with
    | 0 -> raise (System.ArgumentException "Empty string not allowed")
    | 1 -> Leaf(List.head word) // If length is one then must be the last or a leaf
    | _ -> Node(List.head word, false, [ addLeaves (List.tail word) ])



let rec contains (letter: char) (trie: Trie list) =
    let f x =
        match x with
        | Leaf c -> c = letter
        | Node (c, _, _) -> c = letter

    (List.tryFind f trie) <> None


let insert (word: string) trie =
    let rec go cl trie =
        match trie with
        | Leaf ch ->
            match List.length cl with
            | 1 -> trie
            | x -> Node(ch, true, [ addLeaves (List.tail cl) ])
        | Node (ch, isWord, nodeList) ->
            match List.length cl with
            | 1 -> Node(ch, true, nodeList)
            | x ->
                if (contains (cl |> List.tail |> List.head) nodeList) then
                    Node(
                        ch,
                        isWord,
                        List.filter
                            (fun trie ->
                                match trie with
                                | Leaf c -> c = (cl |> List.tail |> List.head)
                                | Node (c, _, _) -> c = (cl |> List.tail |> List.head))
                            nodeList
                        |> List.map (fun trie -> go (List.tail cl) trie)
                    )
                else
                    Node(ch, isWord, nodeList @ [ addLeaves (List.tail cl) ])


    go (List.ofSeq word) trie



let find (letter: char) (xs: Trie list) =
    let f x =
        match x with
        | Leaf c -> c = letter
        | Node (c, _, _) -> c = letter

    (List.tryFind f xs)

let lookup word trie =
    let rec go cl trie =
        match trie with
        | Leaf c ->
            match List.length cl with
            | 1 -> (List.head cl) = c
            | _ -> false
        | Node (c, _, nodeList) ->
            if (c = (List.head cl)) then
                match List.length cl with
                | 1 -> true
                | x ->
                    match find (cl |> List.tail |> List.head) nodeList with
                    | None -> false
                    | Some (n) -> go (List.tail cl) n
            else
                false

    go (List.ofSeq word) trie
