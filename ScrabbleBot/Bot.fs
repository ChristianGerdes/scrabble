namespace BiggerBrainBot

open ScrabbleUtil.Dictionary
open ScrabbleUtil.DebugPrint

module internal Bot =
    open MultiSet
    open ScrabbleUtil

    type Direction =
        | up = 0
        | right = 1
        | down = 2
        | left = 3

    type AnchorPoint = Direction * (int * int)

    type Move = list<(coord * (uint32 * (char * int)))>


    let getUsedTileCoords gamestate =
        match Map.isEmpty gamestate with
        | true -> List.Empty
        | false ->
            gamestate
            |> Map.toSeq
            |> Seq.map (fun (k, _) -> k)
            |> Seq.toList

    let generateCoord coord dx dy = (fst coord + dx, snd coord + dy)

    let generateAnchorPoints usedTileCoords : list<AnchorPoint> =
        // let validPts : List<AnchorPoint> = List.empty
        // let temp = List.empty
        // for coord in usedTileCoords do
        //     // if coord (x, y + 1) not in getUsedTileCoords
        //     if (!List.exists (fst coord, snd coord +1) usedTileCoords) then ((("up"), (fst coord, snd coord+1)) :: temp)
        //         else temp
        // if
        //     (("up"), (fst coord, snd coord+1)) :: temp |> ignore
        //     (("right"), (fst coord+1, snd coord)) :: temp |> ignore
        //     (("down"), (fst coord, snd coord-1)) :: temp |> ignore
        //     (("left"), (fst coord-1, snd coord)) :: temp |> ignore

        // [(x1, y1), (x2, y2), ...]
        let coords =
            List.fold
                (fun acc coord ->
                    [
                      // (Direction.left, generateCoord coord -1 0)
                      (Direction.right, generateCoord coord 1 0)
                      (Direction.down, generateCoord coord 0 1)
                      //   (Direction.up, generateCoord coord 0 -1)
                      ]
                    @ acc)
                []
                usedTileCoords
            |> List.filter
                (fun (_, coord) -> (List.exists (fun (x, y) -> x <> fst coord && y <> snd coord) usedTileCoords))

        coords

    //      X X
    //    X O O X
    //      X X

    //Jespers guideline:
    // Keep a map from coordinates to character (and possibly point values) of whatever you or other players have placed on the board.
    // Make a function that tries to find the best possible move from a single coordinate writing in one direction.
    // For every coordinate check if something is placed on the board, and if so use the step function to take a step in the trie. If successful, recurse (move the coordinate one step in the direction you are writing and use the new dictionary from the step function).
    // if nothing is placed there you need to fold over your hand to try to place something there. Use the step function again, but this time when you recurse make sure to remove that tile from your hand so that you don't use it twice.
    // If the step function ever says that you have found a word then you can return that, or keep looking for a better one (for some definition of better).

    // let extractCharFromGamestate coord (state: State.state) =
    //     (Map.find coord state.gameState) |> snd |> fst

    let extractCurrentFromGamestate coord (state: State.state) =
        Map.find coord state.gameState

    // let extractIdFromGamestate coord (state: State.state) =
        (Map.find coord state.gameState) |> fst


    // 0 0 1A1 1 0 14N1
    let makeMove (state: State.state) (tiles: Map<uint32, tile>) =
        let moveInDirection direction (x, y) =
            match direction with
            | Direction.down -> (x, y+1)
            | Direction.right -> (x+1, y)
            | Direction.up -> (x, y-1)
            | Direction.left -> (x-1, y)
            | _ -> failwith "not implemented yet"


        
        let idToTile id : uint32 = match Map.tryFind id tiles with
                                    | Some v -> v
                                    | None -> failwith "."
        
        let isCoordInUse coord =
            match Map.tryFind coord state.gameState with
            | Some _ -> true
            | None -> false

        let anchorPoints =
            List.collect
                (fun ((x, y), _) ->
                    match ((isCoordInUse (x, y - 1)), (isCoordInUse (x - 1, y))) with
                    | (true, true) -> []
                    | (true, false) -> [ ((x, y), Direction.right) ]
                    | (false, true) -> [ ((x, y), Direction.down) ]
                    | (false, false) ->
                        [ ((x, y), Direction.right)
                          ((x, y), Direction.down) ])
                (Map.toList state.gameState)
            |> function
            | [] ->
                [ (state.board.center, Direction.right)
                  (state.board.center, Direction.down) ]
            | x -> x


        let rec findMove (dict: Dict) hand (currentPoint, currentDirection) (moves: Move) =
            match (Map.tryFind currentPoint state.gameState) with
            | Some (char, pointValue) -> match (step char dict) with 
                                            | Some (b,d) when b -> moves
                                            | Some (b,d) -> findMove d hand ((moveInDirection currentDirection currentPoint), currentDirection) moves
                                            | None -> moves 
            | None -> MultiSet.fold (fun acc id _ -> 
                                        let (char, pv) = idToTile id |> Set.minElement 
                                        
                                        match (step char dict) with
                                        | Some (b,d) when b -> (currentPoint, (id, (char, pv))) :: moves
                                        | Some (b,d) -> (currentPoint, (id, (char, pv))) :: moves
                                        | None -> 
            // ) //moves hand
        let move =
            List.fold (fun acc anchorPoint -> findMove state.dict state.hand anchorPoint acc) [] anchorPoints

        move
    // type Move = list<(coord * (uint32 * (char * int)))>


    // findMove state.dict state.hand coord [] []

    // let makeMove (state: State.state) coord direction =
    //     if(Map.isEmpty state.gameState) then
    //         let rec aux (dict: Dict) hand currentCoord (acc: list<list<coord*(uint32 * (char * int))>>) (localAcc: list<coord*(uint32 * (char * int))>) =
    //             let currentchar = extractCharFromGamestate currentCoord state
    //             let directedCoord = moveInDirection direction currentCoord

    //             if (state.gameState.ContainsKey directedCoord) then
    //                 printf "First if statement entered"
    //                 match(step currentchar dict) with
    //                 | Some (_,d) -> aux d hand directedCoord acc localAcc
    //                 | None -> failwith "not gonna happen"
    //             else
    //                 printf "else statement entered"
    //                 match (step currentchar dict) with
    //                 | Some (b,d) when b ->
    //                     printf "step with bool true"
    //                     localAcc @ [currentCoord, (extractCurrentFromGamestate currentCoord state)]
    //                     localAcc :: acc |> ignore
    //                     printfn "localAcc: %A" localAcc
    //                     localAcc = List.Empty
    //                 | Some (b,d) ->
    //                     printf "step"
    //                     let newHand = removeSingle (extractIdFromGamestate currentCoord state) hand
    //                     localAcc @ [(currentCoord, (extractCurrentFromGamestate currentCoord state))] |> ignore

    //                     aux d newHand directedCoord acc localAcc
    //                     // remove from hand
    //                 | None ->
    //                     printf "None"
    //                     let d = 123
    //                     d = 2
    //                     // failwith "recusrively call with another starting letter?" //reset acc?
    //                     // localAcc


    //         aux state.dict state.hand coord [] []
    //     else
    //         printf "None"
    //         let d = 123
    //         d = 2

// type state = {
//     board         : Parser.board
//     dict          : ScrabbleUtil.Dictionary.Dict
//     playerNumber  : uint32
//     hand          : MultiSet.MultiSet<uint32>
//     gameState     : Map<coord, (uint32 * (char * int))>
// }
