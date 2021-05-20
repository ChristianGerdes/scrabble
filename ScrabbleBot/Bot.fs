namespace BiggerBrainBot

open ScrabbleUtil.Dictionary

module internal Bot =
    open MultiSet
    open ScrabbleUtil

    type Direction =
        | right = 0
        | down = 1

    type Move = list<(coord * (uint32 * (char * int)))>

    let generateMove (state: State.state) (tiles: Map<uint32, tile>) =
        let moveInDirection direction (x, y) =
            match direction with
            | Direction.down -> (x, y + 1)
            | Direction.right -> (x + 1, y)
            | _ -> (x,y)

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
            match size state.hand > count with
            | false -> acc
            | true ->
                match direction with
                | Direction.right when isCoordInUse (x - 1, y) -> acc
                | Direction.down when isCoordInUse (x, y - 1) -> acc
                | Direction.right when
                    not (isCoordInUse (x - 1, y))
                    && not (isCoordInUse (x - 2, y)) ->
                    let newAcc = ((x - 1, y), Direction.right) :: acc

                    backtrace (count + 1u) ((x - 1, y), Direction.right) newAcc
                | Direction.down when
                    not (isCoordInUse (x, y - 1))
                    && not (isCoordInUse (x, y - 2)) ->
                    let newAcc = ((x, y - 1), Direction.down) :: acc

                    backtrace (count + 1u) ((x, y - 1), Direction.down) newAcc
                | _ -> acc

        let anchorPoints =
            let letters =
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

        let rec gatherChars (x, y) writeDirection coordToValidate charToValidate acc =
            match writeDirection with
            | Direction.right ->
                let newAcc =
                    acc
                    @ [ (coordToChar (x, y) coordToValidate charToValidate) ]

                if (isValidateCoordInUse (x, y + 1) coordToValidate) then
                    gatherChars (x, y + 1) writeDirection coordToValidate charToValidate newAcc
                else
                    newAcc
            | Direction.down ->
                let newAcc =
                    acc
                    @ [ (coordToChar (x, y) coordToValidate charToValidate) ]

                if (isValidateCoordInUse (x + 1, y) coordToValidate) then
                    gatherChars (x + 1, y) writeDirection coordToValidate charToValidate newAcc
                else
                    newAcc
            | _ -> acc

        let isValidMove coord =
            if (state.gameState.IsEmpty) then
                true
            else
                match state.board.squares coord with
                | Some _ -> true
                | None -> false

        let isAdjacentWordValid word dict =
            match String.length word with
            | 1 -> true
            | _ -> lookup word dict

        let isTileConnected (x, y) =
            if (isCoordInUse (x - 1, y)) then
                true
            else if (isCoordInUse (x + 1, y)) then
                true
            else if (isCoordInUse (x, y - 1)) then
                true
            else if (isCoordInUse (x, y + 1)) then
                true
            else
                false

        let rec isConnected play =
            List.fold
                (fun acc (coord, _) ->
                    if (isTileConnected coord) then
                        true
                    else
                        acc)
                state.gameState.IsEmpty
                play

        let rec findMove (dict: Dict) hand (currentPoint, currentDirection) (play: Move) (validPlays: list<Move>) =

            match (Map.tryFind currentPoint state.gameState) with
            | Some (char, pointValue) ->
                match (step char dict) with
                | Some (b, d) when
                    b
                    && play.Length > 0
                    && not (isCoordInUse (moveInDirection currentDirection currentPoint)) -> play :: validPlays
                | Some (b, d) ->
                    findMove d hand ((moveInDirection currentDirection currentPoint), currentDirection) play validPlays
                | None -> validPlays
            | None ->
                fold
                    (fun acc id _ ->
                        let (char, pv) = idToTile id |> Set.minElement
                        let edge = findEdge currentPoint currentDirection

                        let charList =
                            gatherChars edge currentDirection currentPoint char []

                        let adjacentWord = System.String.Concat(charList)

                        match isValidMove currentPoint with
                        | true ->
                            match (isAdjacentWordValid adjacentWord state.dict) with
                            | true ->
                                match (step char dict) with
                                | Some (b, d) ->
                                    if
                                        b
                                        && not (isCoordInUse (moveInDirection currentDirection currentPoint))
                                    then
                                        if (isConnected ((currentPoint, (id, (char, pv))) :: play)) then
                                            ((currentPoint, (id, (char, pv))) :: play)
                                            :: validPlays
                                        else
                                            acc
                                    else
                                        findMove
                                            d
                                            (removeSingle id hand)
                                            ((moveInDirection currentDirection currentPoint), currentDirection)
                                            ((currentPoint, (id, (char, pv))) :: play)
                                            acc
                                | _ -> acc
                            | false -> acc
                        | false -> acc)
                    validPlays
                    hand

        let plays =
            List.fold (fun acc anchorPoint -> findMove state.dict state.hand anchorPoint [] acc) [] anchorPoints

        let playScore move =
            List.fold (fun acc (coord, (id, (char, pointValue))) -> acc + pointValue) 0 move

        List.fold
            (fun acc currentPlay ->
                match playScore currentPlay with
                | x when x > playScore acc -> currentPlay
                | _ -> acc)
            []
            plays