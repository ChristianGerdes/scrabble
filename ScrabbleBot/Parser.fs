// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

open ScrabbleUtil // NEW. KEEP THIS LINE.
open System
open Eval
open FParsecLight.TextParser // Industrial parser-combinator library. Use for Scrabble Project.

let pIntToChar = pstring "intToChar"
let pPointValue = pstring "pointValue"

let pCharToInt = pstring "charToInt"
let pToUpper = pstring "toUpper"
let pToLower = pstring "toLower"
let pCharValue = pstring "charValue"

let pTrue = pstring "true"
let pFalse = pstring "false"
let pIsDigit = pstring "isDigit"
let pIsLetter = pstring "isLetter"

let pif = pstring "if"
let pthen = pstring "then"
let pelse = pstring "else"
let pwhile = pstring "while"
let pdo = pstring "do"
let pdeclare = pstring "declare"

let pNot = pstring "~"
let pConjuntion = pstring @"/\"
let pDisjonction = pstring @"\/"
let pEqual = pstring "="
let pNotEqual = pstring "<>"
let pLessThan = pstring "<"
let pLessThanOrEqual = pstring "<="
let pGreaterThan = pstring ">"
let pGreaterThanOrEqual = pstring ">="

let pAss = pstring ":="
let pSeq = pstring ";"

let whitespaceChar =
    satisfy System.Char.IsWhiteSpace <?> "whitespace"

let pletter = satisfy System.Char.IsLetter
let palphanumeric = satisfy System.Char.IsLetterOrDigit
let spaces = many whitespaceChar
let spaces1 = many1 whitespaceChar

// 7.3
let (.>*>.) a b = (a .>> spaces) .>>. b
let (.>*>) a b = (a .>> spaces) .>> b
let (>*>.) a b = (a .>> spaces) >>. b

let parenthesise p = pchar '(' >*>. p .>*> pchar ')'

let convert =
    fun (tuple: (char * char List)) ->
        (fst (tuple)) :: (snd (tuple))
        |> System.String.Concat

let pid =
    pletter <|> pchar ('_')
    .>>. many (palphanumeric <|> pchar ('_'))
    |>> convert

let unop p1 p2 = p1 >*>. p2
let binop mid p1 p2 = p1 .>*> mid .>*>. p2

let TermParse, tref = createParserForwardedToRef<aExp> ()
let ProdParse, pref = createParserForwardedToRef<aExp> ()
let AtomParse, aref = createParserForwardedToRef<aExp> ()
let CharParse, cref = createParserForwardedToRef<cExp> ()

//
let AddParse =
    binop (pchar '+') ProdParse TermParse |>> Add
    <?> "Add"

let SubParse =
    binop (pchar '-') ProdParse TermParse |>> Sub
    <?> "Sub"

do tref := choice [ AddParse; SubParse; ProdParse ]

let MulParse =
    binop (pchar '*') AtomParse ProdParse |>> Mul
    <?> "Mul"

let ModParse =
    binop (pchar '%') AtomParse ProdParse |>> Mod
    <?> "Mod"

let DivParse =
    binop (pchar '/') AtomParse ProdParse |>> Div
    <?> "Div"

do
    pref
    := choice [ MulParse
                DivParse
                ModParse
                AtomParse ]


let NParse = pint32 |>> N <?> "Int"
let VParse = pid |>> V <?> "Var"
let ParParse = parenthesise TermParse <?> "Par"

let PVParse =
    unop pPointValue (parenthesise TermParse) |>> PV
    <?> "PV"

let NegParse =
    unop (pchar '-') ProdParse
    |>> (fun aexp -> (N -1) .*. aexp)
    <?> "Neg"

let CharToIntParse =
    unop pCharToInt CharParse |>> CharToInt
    <?> "charToInt"

do
    aref
    := choice [ NegParse
                NParse
                ParParse
                CharToIntParse
                PVParse
                VParse ]

//


let AexpParse = TermParse


//7.9 Cexp Parse
let singleQuoteParse = pchar (''')

let charValueParse =
    unop (pCharValue) TermParse |>> CV <?> "charValue"

let intToCharParse =
    unop pIntToChar TermParse |>> IntToChar
    <?> "intToChar"

let toUpperParse =
    unop pToUpper (parenthesise CharParse) |>> ToUpper
    <?> "toUpper"

let toLowerParse =
    unop pToLower (parenthesise CharParse) |>> ToLower
    <?> "toLower"

let charParse =
    singleQuoteParse
    >>. choice [ pletter; whitespaceChar ]
    .>> singleQuoteParse
    |>> C
    <?> "char"

let parParse = parenthesise CharParse <?> "Par"

do
    cref
    := choice [ parParse
                toUpperParse
                toLowerParse
                intToCharParse
                charValueParse
                charParse ]

let CexpParse = CharParse


// 7.10 Bexp parse
let bTermParse, bTref = createParserForwardedToRef<bExp> ()
let bProdParse, bPref = createParserForwardedToRef<bExp> ()
let bAtomParse, bAref = createParserForwardedToRef<bExp> ()

let ConjunctionParse =
    binop pConjuntion bProdParse bTermParse |>> Conj
    <?> "Conj"

let DisjuctionParse =
    binop pDisjonction bProdParse bTermParse
    |>> (fun (a, b) -> (a .||. b))
    <?> "Dis"

do
    bTref
    := choice [ ConjunctionParse
                DisjuctionParse
                bProdParse ]

let EqualParse =
    binop pEqual AtomParse ProdParse |>> AEq
    <?> "Equal"

let NotEqualParse =
    binop pNotEqual AtomParse ProdParse
    |>> (fun (a, b) -> (a .<>. b))
    <?> "NotEqual"

let LessThanParse =
    binop pLessThan AtomParse ProdParse |>> ALt
    <?> "LessThan"

let LessThanOrEqualParse =
    binop pLessThanOrEqual AtomParse ProdParse
    |>> (fun (a, b) -> (a .<=. b))
    <?> "LessThanOrEqual"

let GreaterThanParse =
    binop pGreaterThan AtomParse ProdParse
    |>> (fun (a, b) -> (a .>. b))
    <?> "GreaterThan"

let GreaterThanOrEqualParse =
    binop pGreaterThanOrEqual AtomParse ProdParse
    |>> (fun (a, b) -> (a .>=. b))
    <?> "GreaterThanOrEqual"

do
    bPref
    := choice [ EqualParse
                NotEqualParse
                LessThanParse
                LessThanOrEqualParse
                GreaterThanParse
                GreaterThanOrEqualParse
                bAtomParse ]


let NotParse = unop pNot bTermParse |>> (~~) <?> "Not"

let LetterParse =
    unop pIsLetter CharParse |>> IsLetter
    <?> "IsLetter"

let DigitParse =
    unop pIsDigit CharParse |>> IsDigit <?> "isDigit"

let TrueParse = pTrue |>> (fun _ -> TT) <?> "True"
let FalseParse = pFalse |>> (fun _ -> FF) <?> "False"
let bParParse = parenthesise bTermParse <?> "Par"

do
    bAref
    := choice [ TrueParse
                FalseParse
                NotParse
                LetterParse
                DigitParse
                bParParse ]

let BexpParse = bTermParse


// 7.11 stmt parse
let stmtTopParse, stmtRef = createParserForwardedToRef<stm> ()
let stmtSeqParse, stmtSeq = createParserForwardedToRef<stm> ()
let pBracket p = pchar '{' >*>. p .>*> pchar '}'



let WhileParse =
    pwhile >*>. parenthesise BexpParse .>*> pdo
    .>*>. pBracket stmtSeqParse
    |>> While
    <?> "While"

let AssParse =
    pid .>*> pAss .>*>. AexpParse |>> Ass
    <?> "Assignment"

let SeqParse =
    stmtTopParse .>*> pSeq .>*>. stmtSeqParse |>> Seq
    <?> "Sequence"

let DeclareParse =
    pdeclare .>> whitespaceChar >>. pid |>> Declare
    <?> "Declare"

let IfThenParse =
    pif >*>. parenthesise BexpParse .>*> pthen
    .>*>. pBracket stmtSeqParse
    |>> (fun (a, b) -> ITE(a, b, Skip))
    <?> "if-then"

let IfThenElseParse =
    (pif >*>. parenthesise BexpParse)
    .>*>. (pthen >*>. pBracket stmtSeqParse)
    .>*>. (pelse >*>. pBracket stmtSeqParse)
    |>> (fun (a, b) -> ITE(fst a, snd a, b))
    <?> "if-then-else"

do stmtSeq := choice [ SeqParse; stmtTopParse ]

do
    stmtRef
    := choice [ DeclareParse
                AssParse
                WhileParse
                IfThenElseParse
                IfThenParse ]

let stmntParse = stmtSeqParse

(* The rest of your parser goes here *)

type word = (char * int) list

type square = Map<int, word -> int -> int -> int>

let parseSquareFun =
    (fun sqp -> (Map.map (fun key sc -> stmntToSquareFun (getSuccess (run stmntParse sc))) sqp))

let parseBoardFun =
    fun s m -> stmntToBoardFun (getSuccess (run stmntParse s)) m


type boardFun = coord -> square option

type board =
    { center: coord
      defaultSquare: square
      squares: boardFun }

let parseBoardProg (bp: boardProg) =
    let mapped =
        bp.squares
        |> Map.map (fun _ y -> parseSquareFun y)

    let defSq = mapped.Item(bp.usedSquare)

    { defaultSquare = defSq
      squares = parseBoardFun bp.prog mapped
      center = bp.center }
