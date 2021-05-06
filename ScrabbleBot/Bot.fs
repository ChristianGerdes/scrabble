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
    type Direction = up = 0 | right = 1 | down = 2 | left = 3
    type AnchorPoint = Direction * (int * int)

    // 1. overview of tiles available
        // nothing is placed ->      place in center for now?
            //find word from current hand
        // something is placed ->   have to build from placed tiles
                // Start looking a down and right direction
                //find word from current hand from each char
                // figure out which direction word grows in


    let getUsedTileCoords gamestate =
        match Map.isEmpty gamestate with
        | true -> List.Empty
        | false -> gamestate |> Map.toSeq |> Seq.map (fun (k,_) -> k) |> Seq.toList 

    let generateCoord coord dx dy = (fst coord + dx, snd coord + dy)

    let generateAnchorPoints usedTileCoords : list<AnchorPoint>=
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
        let coords = List.fold (fun acc coord -> [ (Direction.left, generateCoord coord -1 0); (Direction.right, generateCoord coord 1 0); (Direction.down, generateCoord coord 0 -1); (Direction.up, generateCoord coord 0 1)] @ acc) [] usedTileCoords
                                |> List.filter (fun (_, coord) -> (List.exists (fun x -> x <> coord) usedTileCoords))

        coords
    
//      X X    
//    X O O X
//      X X

    // let findWordByCoord coord direction state =
    //     //Kig på coord og se om der ligger en brik
    //         // true recursive call
    //         // folde over hånden
    //     let rec aux acc best coord dict hand =

    let generateMove (st: State.state) (tiles: Map<uint32, tile>) =

        generateAnchorPoints (getUsedTileCoords st.gameState)
        


