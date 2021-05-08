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

        let isValidMove (x, y) char direction =
            true
            // let candidates = [(x-1, y); (x+1, y); (x, y-1); (x, y+1)]


            // match ((isCoordInUse (x-1, y)), (isCoordInUse (x+1, y)), (isCoordInUse (x, y-1)), (isCoordInUse (x, y+1))) with
            // | (false, false, false, false) -> true
            // | _ -> true

        let rec findMove (dict: Dict) hand (currentPoint, currentDirection) (moves: Move) bestMove =
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

                        // Insert check function
                        match (isValidMove currentPoint char currentDirection) with
                        | true -> match (step char dict) with
                                    | Some (b, d) ->
                                        if b && not (isCoordInUse (moveInDirection currentDirection currentPoint)) then
                                            (currentPoint, (id, (char, pv))) :: moves
                                        else
                                            findMove
                                                d
                                                (removeSingle id hand)
                                                ((moveInDirection currentDirection currentPoint), currentDirection)
                                                ((currentPoint, (id, (char, pv))) :: moves)
                                                acc
                                    | _ -> bestMove
                        | false -> acc
                    )
                    moves
                    hand

        let move =
            List.fold
                (fun acc anchorPoint ->
                    printf "%A\n" anchorPoint
                    findMove state.dict state.hand anchorPoint [] acc)
                []
                anchorPoints

        move
