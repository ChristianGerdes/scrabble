namespace BiggerBrainBot

    // type state = {
    //     board         : Parser.board
    //     dict          : ScrabbleUtil.Dictionary.Dict
    //     playerNumber  : uint32
    //     hand          : MultiSet.MultiSet<uint32>
    //     gameState     : Map<coord, (uint32 * (char * int))>
    // }





let generateMove (st: State.state) (tiles: Map<uint32, tile>) =
    // val placedTiles : list<coord * Direction>
    //hand dict gamestate
    // hand = [1, 2, 3] [E, H, J] => A
    let rec aux =
        

