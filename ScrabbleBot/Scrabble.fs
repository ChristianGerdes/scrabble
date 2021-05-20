namespace BiggerBrainBot

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open System.IO
open ScrabbleUtil.DebugPrint
open Bot

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)

        if m.Success then
            Some(List.tail [ for g in m.Groups -> g.Value ])
        else
            None

    let parseMove ts =
        let pattern =
            @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?"

        Regex.Matches(ts, pattern)
        |> Seq.cast<Match>
        |> Seq.map
            (fun t ->
                match t.Value with
                | Regex pattern [ x; y; id; c; p ] -> ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)")
        |> Seq.toList

module Print =

    let printHand pieces hand =
        hand
        |> MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module Scrabble =
    open System.Threading

    let playGame cstream pieces (st: State.state) =

        let rec aux (st: State.state) =
            Print.printHand pieces (State.hand st)

            if (st.players.[0] = st.playerNumber) then
                let move = generateMove st pieces

                match move.IsEmpty with
                | true -> send cstream (SMChange(st.hand |> MultiSet.toList))
                | false -> send cstream (SMPlay move)

            let shiftedPlayers =
                match st.players with
                | head :: tail -> tail @ [ head ]
                | [] -> []

            let rec removePlayer id players =
                match players with
                | head :: tail when head = id -> tail
                | head :: tail -> removePlayer id (tail @ [ head ])
                | _ -> players

            let msg = recv cstream

            match msg with
            | RCM (CMPlaySuccess (ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let newHand = State.updateHand st.hand ms newPieces

                let newGameState = State.updateGameState ms st.gameState //update to gamestate

                let st' =
                    State.mkState st.board st.dict newHand newGameState shiftedPlayers st.playerNumber // This state needs to be updated

                aux st'
            | RCM (CMChangeSuccess (newTiles)) ->
                let newHand =
                    State.updateHand MultiSet.empty [] newTiles

                let st' =
                    State.mkState st.board st.dict newHand st.gameState shiftedPlayers st.playerNumber

                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                debugPrint (sprintf "Player %d <- Made play" (State.playerNumber st))
                (* Successful play by other player. Update your state *)
                let newGameState = State.updateGameState ms st.gameState

                let st' =
                    State.mkState st.board st.dict st.hand newGameState shiftedPlayers st.playerNumber

                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                debugPrint (sprintf "Player %d <- Failed" (State.playerNumber st))
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated

                aux st'
            | RCM (CMPassed (pid)) ->
                let st' =
                    State.mkState st.board st.dict st.hand st.gameState shiftedPlayers st.playerNumber

                aux st'
            | RCM (CMForfeit (pid)) ->
                let updatedPlayers = removePlayer pid st.players

                let st' =
                    State.mkState st.board st.dict st.hand st.gameState updatedPlayers st.playerNumber

                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err ->
                // printfn "Gameplay Error:\n%A" err
                aux st

        aux st

    let startGame
        (boardP: boardProg)
        (dictf: bool -> Dictionary.Dict)
        (numPlayers: uint32)
        (playerNumber: uint32)
        (playerTurn: uint32)
        (hand: (uint32 * uint32) list)
        (tiles: Map<uint32, tile>)
        (timeout: uint32 option)
        (cstream: Stream)
        =
        debugPrint (
            sprintf
                "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n"
                numPlayers
                playerNumber
                playerTurn
                hand
                timeout
        )

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.parseBoardProg boardP

        let handSet =
            List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        let players = [ 1u .. numPlayers ]

        let rec rotate players =
            match players with
            | head :: tail when head = playerTurn -> players
            | head :: tail -> rotate (tail @ [ head ])
            | [] -> []

        let rotatedPlayers = rotate players

        fun () -> playGame cstream tiles (State.mkState board dict handSet Map.empty rotatedPlayers playerNumber)
