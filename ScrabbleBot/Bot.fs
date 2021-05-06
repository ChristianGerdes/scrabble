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



    // 1. overview of tiles available
        // nothing is placed ->      place in center for now?
            //find word from current hand
        // something is placed ->   have to build from placed tiles
                // Start looking a down and right direction
                //find word from current hand from each char
                // figure out which direction word grows in

    let generateMove (st: State.state) (tiles: Map<uint32, tile>) =
        st.gameState.Add(((0,0), (1u, ('A', 1)))) |> ignore

        let checkGameState gamestate =
                Map.isEmpty gamestate

        let getUsedTileCoords gamestate =
                match Map.isEmpty gamestate with
                | true -> List.Empty
                | false -> gamestate |> Map.toSeq |> Seq.toList

        getUsedTileCoords st.gameState





    // let generateMove (st: State.state) (tiles: Map<uint32, tile>) =
    //     // val placedTiles : list<coord * Direction>
    //     // hand dict gamestate
    //     // hand = [1, 2, 3] [E, H, J] => A
    //     let findTile id : tile=
    //         match Map.tryFind id tiles with
    //         | Some v -> v
    //         | None -> failwith "Cannot happen"

    //     let tileToChar tile : char = fst (Set.minElement tile)
    //     let tileToPoint tile : int = snd (Set.minElement tile)
    //     let idToChar id : char = (tileToChar (findTile id))
    //     let idToPoint id : int = (tileToPoint (findTile id))



        // if(gameState == null) {
        //     //fresh start
               //find best possible move that starts in center
        // }
        //  else {
        //      work from there
        //  }

        // let currentHand (hand : MultiSet.MultiSet<uint32>) (moves: list<(int * int) * (uint32 * (char * int))>) =
        //     subtract hand (ofList (List.map (fun (x,y) -> (fst y)) moves))

        // // MultiSet<uint32> ->  Dict -> list<(int * int) * (uint32 * (char * int))> ->
        // let rec aux (hand: MultiSet<uint32>) (dict: Dict) (moves: list<(int * int) * (uint32 * (char * int))>) : list<(int * int) * (uint32 * (char * int))> option =
        //     // (printfn "Hand %A - moves %A" ((List.map (fun f -> tileToChar (findTile f)) (toList hand))) moves)
        //     (printfn "moves %A" moves)
        //     (printfn "Current hand %A" ((List.map (fun f -> tileToChar (findTile f)) (toList (currentHand st.hand moves)))))

        //     // let test = List.fold (fun acc i -> [i] @ acc) [] [1; 2; 3]

        //     // printf "%A" test

        //     // [B, C, U, K]
        //     // B -> [C, U, K]
        //     // C, U -> [C, K]
        //     // C -> [K]
        //     // K -> [], acc = [(0, 0) 11, ("K", 5)]

        //     List.fold (fun (acc: list<(int * int) * (uint32 * (char * int))> option) id ->
        //         match (step (idToChar id) dict) with
        //         | None -> acc
        //             // acc
        //             // (printfn "No match. %A - Tile: %A - Hand: %A" (tileToChar (findTile id)) (findTile id) (removeSingle id hand))
        //             // aux (removeSingle id hand) dict acc
        //         | Some (b, d) when b ->
        //             // (printfn "Found valid word. Hand: %A\n" hand)
        //             acc @ [((0, 0), (id, ((idToChar id), (idToPoint id))))]
        //             // acc @ [((0, 0), (id, ((idToChar id), (tileToPoint (findTile id)))))]
        //         | Some (b, d) ->
        //             // acc
        //             // (printfn "Found invalid word. Char: %A\n" (idToChar id))
        //             // (moves @ (aux (currentHand st.hand moves) d moves))
        //             let acc1 = acc @ [((0, 0), (id, ((idToChar id), (idToPoint id))))]

        //             let output = aux (removeSingle id hand) d acc1

        //             match output with
        //             | Some (moves, valid) when valid -> acc @ moves
        //             | Some (moves, valid) -> acc
        //             // | x -> acc @ [x]

        //             printf "OUTPUT: %A\n" output

        //             output
        //     ) moves (toList hand)

        // aux st.hand st.dict []

            //Look at first letter
            // if letter exists in dict:
                // Found and valid  (bool = true):
                    // Append letter to acc -> word is done
                // Found not valid (bool = false):
                    // Recursive call aux with currentHand (full hand - acc), nested dict, and acc with appended letter
            // else:

