namespace BiggerBrainBot
open MultiSet


module internal State =
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.
    type coord = int * int

    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        gameState     : Map<coord, (char * int)>
    }

    let mkState b d pn h gs = {board = b; dict = d;  playerNumber = pn; hand = h ; gameState = gs}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let gameState st     = st.gameState

    let updateHand (hand: MultiSet<uint32>) (moves: list<coord * (uint32 * (char * int))>) (newPcs: list<uint32 * uint32>) =
        let used = List.map (fun m -> fst (snd m)) moves |> ofList

        let updatedHand = subtract hand used

        List.fold (fun acc (id, count) -> add id count acc) updatedHand newPcs

    let updateGameState (moves: list<coord * (uint32 * (char * int))>) (previousState: Map<coord, (char * int)>) =
    // let updateGameState moves previousState =
        List.fold (fun a b -> Map.add (fst b) (snd (snd b)) a) previousState moves