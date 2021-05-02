namespace BiggerBrainBot
open ScrabbleUtil.Dictionary
open ScrabbleUtil.DebugPrint

module internal Bot =
    open MultiSet
    open ScrabbleUtil
    // type state = {
    //     board         : Parser.board
    //     dict          : ScrabbleUtil.Dictionary.Dict
    //     playerNumber  : uint32
    //     hand          : MultiSet.MultiSet<uint32>
    //     gameState     : Map<coord, (uint32 * (char * int))>
    // }

    let generateMove (st: State.state) (tiles: Map<uint32, tile>) =
        // val placedTiles : list<coord * Direction>
        // hand dict gamestate
        // hand = [1, 2, 3] [E, H, J] => A
        let findTile id : tile=
            match Map.tryFind id tiles with
            | Some v -> v
            | None -> failwith "Cannot happen"

        let tileToChar tile : char = fst (Set.minElement tile)
        let tileToPoint tile : int = snd (Set.minElement tile)

        // [1, 2, 3]
        //["H", "E", "J"]

        // [(0, 0), "H"]
        // [(1, 0), "E"]
        // [(2, 0), "J"]

        // [(0, 0), "H"]
        // [(0, 0), "E"]
        // [(0, 0), "J"]

        let rec aux (hand: MultiSet<uint32>) (dict: Dict) (acc: list<(int * int) * (uint32 * (char * int))>) =
            let test = hand |> fold (fun _ x i ->
                                printf "%A" (tileToChar (findTile i))
                            ) ()

            printf "\n\n"

            match (toList hand) with
            | [] -> acc
            | head::tail ->
                match (step (tileToChar (findTile head)) dict) with
                | None -> acc
                | Some (b, d) when b ->
                    (acc @ [((acc.Length, 0), (head, ((tileToChar (findTile head)), (tileToPoint (findTile head)))))])
                | Some (b, d) ->
                    aux (ofList tail) d (acc @ [((acc.Length, 0), (head, ((tileToChar (findTile head)), (tileToPoint (findTile head)))))])

        // let rec aux (hand: MultiSet<uint32>) (dict: Dict) (acc: list<(int * int) * (uint32 * (char * int))>) =
        //     match (toList hand) with
        //     | [] -> acc
        //     | head::tail ->
        //         match (step (tileToChar (findTile head)) dict) with
        //         | None -> acc
        //         | Some (b, d) when b ->
        //             (acc @ [((acc.Length, 0), (head, ((tileToChar (findTile head)), (tileToPoint (findTile head)))))])
        //         | Some (b, d) ->
        //             aux (ofList tail) d (acc @ [((acc.Length, 0), (head, ((tileToChar (findTile head)), (tileToPoint (findTile head)))))])

        // [1, 2, 3] [4, 5] => [1, 2, 3, 4, 5]
        // [1, 2, 3] [4, 5] => [1, 2, 3, [4, 5]]

        // [1, 2, 3]
        // [2, 3]
        // [3]
        // [2, 1, 3]
        // [1, 3]

        // let rec aux (hand: MultiSet<uint32>) (dict: Dict) (acc: list<(int * int) * (uint32 * (char * int))>) =
        //     // (printfn "Hand %A - Acc %A" ((List.map (fun f -> tileToChar (findTile f)) (toList hand))) acc)
        // // multiset.fold
        //     match (toList hand) with
        //     | [] -> acc
        //     | head::tail ->
        //         match (step (tileToChar (findTile head)) dict) with
        //         | None -> acc
        //         | Some (b, d) when b ->
        //             // (printfn "Found word ending %A %A" acc (tileToChar (findTile head)))

        //             let move = ((acc.Length, 0), (head, ((tileToChar (findTile head)), (tileToPoint (findTile head)))))

        //             (acc @ [move])
        //         | Some (b, d) ->
        //             // (printfn "Going deeper %A" (tileToChar (findTile head)))

        //             let move = ((acc.Length, 0), (head, ((tileToChar (findTile head)), (tileToPoint (findTile head)))))

        //             aux (ofList tail) d (acc @ [move])

        aux st.hand st.dict []


