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

    type Move = list<(coord * (uint32 * (char * int)))>

    let generateMove (state: State.state) (tiles: Map<uint32, tile>) =
        let moveInDirection direction (x, y) =
            match direction with
            | Direction.down -> (x, y + 1)
            | Direction.right -> (x + 1, y)
            | Direction.up -> (x, y - 1)
            | Direction.left -> (x - 1, y)
            | _ -> failwith "not implemented yet"

        let idToTile id =
            match Map.tryFind id tiles with
            | Some v -> v
            | None -> failwith "."

        let isCoordInUse coord =
            match Map.tryFind coord state.gameState with
            | Some _ -> true
            | None -> false

        let isValidateCoordInUse coord coordToValidate =
            match coord with
            | c when c = coordToValidate -> true
            | _ -> isCoordInUse coord

        let rec backtrace count ((x, y), direction) acc =
            // printfn "Backtracing"

            match 3u > count with
            | false -> acc
            | true ->
                match direction with
                | Direction.right when isCoordInUse (x - 1, y) -> acc
                | Direction.down when isCoordInUse (x, y - 1) -> acc
                | Direction.right when
                    not (isCoordInUse (x - 1, y))
                    && not (isCoordInUse (x - 2, y)) ->
                    let newAcc = ((x - 1, y), Direction.right) :: acc

                    // printfn "X %A" count

                    backtrace (count + 1u) ((x - 1, y), Direction.right) newAcc
                | Direction.down when
                    not (isCoordInUse (x, y - 1))
                    && not (isCoordInUse (x, y - 2)) ->
                    let newAcc = ((x, y - 1), Direction.down) :: acc

                    // printfn "Y %A" count

                    backtrace (count + 1u) ((x, y - 1), Direction.down) newAcc
                | _ -> acc

        let anchorPoints =
            // List.fold () acc list
            let letters =
                List.collect
                    (fun ((x, y), _) ->
                        // match ((isCoordInUse (x, y - 1)), (isCoordInUse (x, y - 2))) with
                        // | (false, false) ->
                        //     [ ((x, y-1), Direction.down)
                        //       ((x, y-2), Direction.down) ]
                        // | _ -> []

                        match ((isCoordInUse (x, y - 1)), (isCoordInUse (x - 1, y))) with
                        | (true, true) -> []
                        | (true, false) -> [ ((x, y), Direction.right) ]
                        | (false, true) -> [ ((x, y), Direction.down) ]
                        | (false, false) ->
                            [ ((x, y), Direction.right)
                              ((x, y), Direction.down) ])
                    (Map.toList state.gameState)

            let points =
                List.fold (fun acc anchorPoint -> backtrace 0u anchorPoint acc) letters letters

            match points with
            | [] ->
                let (x, y) = state.board.center

                [ ((x - 1, y), Direction.right)
                  ((x, y - 1), Direction.down)
                  (state.board.center, Direction.right)
                  (state.board.center, Direction.down) ]
            | x -> x

        // |> List.collect
        //     (fun ((x, y), direction) ->
        //         match direction with
        //         | Direction.right when not (isCoordInUse (x-1, y)) -> [((x-1, y), Direction.right)]
        //         | Direction.down when not (isCoordInUse (x, y-1)) -> [((x, y-1), Direction.down)]
        //         | _ -> failwith "HAHA"
        //     )

        let rec findEdge (x, y) direction =
            match direction with
            | Direction.right when isCoordInUse (x, y - 1) -> findEdge (x, y - 1) direction
            | Direction.down when isCoordInUse (x - 1, y) -> findEdge (x - 1, y) direction
            | _ -> (x, y)

        let coordToChar coord coordToValidate charToValidate =
            match coord with
            | c when c = coordToValidate -> charToValidate
            | _ ->
                match Map.tryFind coord state.gameState with
                | Some v -> fst v
                | None -> failwith "No char at tile"

        //    let gatherChars coord direction = failwith "ost"// (perpendicular) up and down for direction collect to list --> string - dict.lookup
        //create string -> lookup combinations of words instead of stepping through

        let rec gatherChars (x, y) writeDirection coordToValidate charToValidate acc =
            match writeDirection with
            | Direction.right ->
                let newAcc =
                    (coordToChar (x, y) coordToValidate charToValidate)
                    :: acc

                if (isValidateCoordInUse (x, y + 1) coordToValidate) then
                    gatherChars (x, y + 1) writeDirection coordToValidate charToValidate newAcc
                else
                    newAcc
            | Direction.down ->
                let newAcc =
                    (coordToChar (x, y) coordToValidate charToValidate)
                    :: acc

                if (isValidateCoordInUse (x + 1, y) coordToValidate) then
                    gatherChars (x + 1, y) writeDirection coordToValidate charToValidate newAcc
                else
                    newAcc
            | _ -> []
        //      T
//      Z
// X X  X X X
//      Y

        let isValidMove coord =
            if (state.gameState.IsEmpty = true) then
                true
            else
                match state.board.squares coord with
                | Some _ -> true
                | None -> false

        // dict.lookup ((TZ) + X + (Y))

        // let rec isValidMove (dict: Dict) (x, y) validateCoord char direction =
        //     match state.board.squares (x, y) with
        //     | Some _ ->
        //         match state.gameState.IsEmpty with
        //         | true -> true
        //         | _ ->
        //             match direction with
        //             | Direction.right ->
        //                 // printfn "Char to step into: %A" (coordToChar (x, y) validateCoord char)
        //                 match (step (coordToChar (x, y) validateCoord char) dict) with
        //                 | Some (b, d) when
        //                     b
        //                     && not (isValidateCoordInUse (x, y + 1) validateCoord) ->
        //                     // printfn "Found valid move %A %A " char validateCoord
        //                     true
        //                 | Some (b, d) when b && isValidateCoordInUse (x, y+1) validateCoord -> isValidMove d (x, y+1) validateCoord char direction
        //                 | Some (b, d) when
        //                     not b
        //                     && isValidateCoordInUse (x, y + 1) validateCoord ->
        //                     isValidMove d (x, y + 1) validateCoord char direction
        //                 | _ -> false
        //             | Direction.down ->
        //                 // printfn "Char to step into: %A" (coordToChar (x, y) validateCoord char)

        //                 match (step (coordToChar (x, y) validateCoord char) dict) with
        //                 | Some (b, d) when b && isValidateCoordInUse (x + 1, y) validateCoord -> isValidMove d (x + 1, y) validateCoord char direction
        //                 | Some (b, d) when
        //                     b
        //                     && not (isValidateCoordInUse (x + 1, y) validateCoord) ->
        //                     // printfn "Found valid move %A %A" char validateCoord

        //                     true
        //                 | Some (b, d) when
        //                     not b
        //                     && isValidateCoordInUse (x + 1, y) validateCoord ->
        //                     isValidMove d (x + 1, y) validateCoord char direction
        //                 | _ -> false
        //             | _ -> false
        //     | None -> false

        let isWordValid word dict =
            match String.length word with
            | 1 -> true
            | _ -> lookup word dict

        let rec findMove (dict: Dict) hand (currentPoint, currentDirection) (moves: Move) bestMove =
            // printfn "Find move"

            match (Map.tryFind currentPoint state.gameState) with
            | Some (char, pointValue) ->
                match (step char dict) with
                | Some (b, d) when b && moves.Length > 0 -> moves
                | Some (b, d) ->
                    findMove d hand ((moveInDirection currentDirection currentPoint), currentDirection) moves bestMove
                | None -> moves
            | None ->
                fold
                    (fun acc id _ ->
                        let (char, pv) = idToTile id |> Set.minElement

                        let edge = findEdge currentPoint currentDirection

                        let charList =
                            gatherChars edge currentDirection currentPoint char []
                        // let word = List.fold (fun acc c -> acc + c) "" charList
                        let word = System.String.Concat(charList)
                        // printfn "%A word: " word
                        // printfn "%A valid: " (lookup word dict)
                        // printfn "Char: %A - EDGE: %A" char edge
                        // Insert check function

                        match isValidMove currentPoint with
                        | true ->
                            match (isWordValid word dict) with
                            | true ->
                                match (step char dict) with
                                | Some (b, d) ->
                                    if
                                        b
                                        && not (isCoordInUse (moveInDirection currentDirection currentPoint))
                                    then
                                        (currentPoint, (id, (char, pv))) :: moves

                                    // let currentMove = (currentPoint, (id, (char, pv))) :: moves
                                    // match bestMove with
                                    // | _ when currentMove.Length <= (List.length bestMove) -> bestMove
                                    // | _ -> currentMove
                                    else
                                        findMove
                                            d
                                            (removeSingle id hand)
                                            ((moveInDirection currentDirection currentPoint), currentDirection)
                                            ((currentPoint, (id, (char, pv))) :: moves)
                                            acc
                                | _ -> bestMove
                            | false -> acc
                        | false -> acc)
                    bestMove
                    hand
        //Check om findMove adder til moves listen

        List.fold
            (fun acc anchorPoint ->
                // printf "%A\n" anchorPoint
                findMove state.dict state.hand anchorPoint [] acc)
            []
            anchorPoints
