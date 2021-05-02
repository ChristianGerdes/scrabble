namespace BiggerBrainBot

open State
type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        gameState     : Map<coord, (uint32 * (char * int))>
    }

let empty = mkState