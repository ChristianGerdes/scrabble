// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

open StateMonad

let add a b =
    a >>= (fun x -> b >>= (fun y -> ret (x + y)))

let sub a b =
    a >>= (fun x -> b >>= (fun y -> ret (x - y)))

let mul a b =
    a >>= (fun x -> b >>= (fun y -> ret (x * y)))

let div a b =
    a
    >>= (fun x ->
        b
        >>= (fun y ->
            if (y <> 0) then
                ret (x / y)
            else
                fail (DivisionByZero)))

let modulo a b =
    a
    >>= (fun x ->
        b
        >>= (fun y ->
            if (y <> 0) then
                ret (x % y)
            else
                fail (DivisionByZero)))

let upper a =
    a >>= (fun x -> ret (System.Char.ToUpper x))

let lower a =
    a >>= (fun x -> ret (System.Char.ToLower x))

let aeq a b =
    a >>= (fun x -> b >>= (fun y -> ret (x = y)))

let alt a b =
    a >>= (fun x -> b >>= (fun y -> ret (x < y)))

let conj a b =
    a >>= (fun x -> b >>= (fun y -> ret (x && y)))

type aExp =
    | N of int
    | V of string
    | WL
    | PV of aExp
    | Add of aExp * aExp
    | Sub of aExp * aExp
    | Mul of aExp * aExp
    | Div of aExp * aExp
    | Mod of aExp * aExp
    | CharToInt of cExp

and cExp =
    | C of char (* Character value *)
    | CV of aExp (* Character lookup at word index *)
    | ToUpper of cExp
    | ToLower of cExp
    | IntToChar of aExp

type bExp =
    | TT (* true *)
    | FF (* false *)

    | AEq of aExp * aExp (* numeric equality *)
    | ALt of aExp * aExp (* numeric less than *)

    | Not of bExp (* boolean not *)
    | Conj of bExp * bExp (* boolean conjunction *)

    | IsVowel of cExp (* check for vowel *)
    | IsConsonant of cExp (* check for constant *)

    | IsLetter of cExp (* check for letter *)
    | IsDigit of cExp (* check for digit *)

let (.+.) a b = Add(a, b)
let (.-.) a b = Sub(a, b)
let (.*.) a b = Mul(a, b)
let (./.) a b = Div(a, b)
let (.%.) a b = Mod(a, b)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj(b1, b2)

let (.||.) b1 b2 =
    ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)

let (.->.) b1 b2 =
    (~~b1) .||. b2 (* boolean implication *)

let (.=.) a b = AEq(a, b)
let (.<.) a b = ALt(a, b)
let (.<>.) a b = ~~(a .=. b)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b)

let (.>=.) a b =
    ~~(a .<. b) (* numeric greater than or equal to *)

let (.>.) a b =
    ~~(a .=. b)
    .&&. (a .>=. b) (* numeric greater than *)

let rec arithEval a : SM<int> =
    match a with
    | N n -> ret n
    | V v -> lookup v
    | WL -> wordLength
    | PV pv -> (arithEval pv) >>= pointValue
    | Add (a, b) -> add (arithEval a) (arithEval b)
    | Sub (a, b) -> sub (arithEval a) (arithEval b)
    | Mul (a, b) -> mul (arithEval a) (arithEval b)
    | Div (a, b) -> div (arithEval a) (arithEval b)
    | Mod (a, b) -> modulo (arithEval a) (arithEval b)
    | CharToInt (c) -> charEval c >>= (fun x -> ret (int x))

and charEval c : SM<char> =
    match c with
    | C c -> ret c
    | CV cv -> arithEval cv >>= characterValue
    | ToUpper c -> charEval c |> upper
    | ToLower c -> charEval c |> lower
    | IntToChar n -> arithEval n >>= (fun x -> ret (char x))

and boolEval b : SM<bool> =
    match b with
    | TT -> ret true
    | FF -> ret false
    | AEq (a, b) -> aeq (arithEval a) (arithEval b)
    | ALt (a, b) -> alt (arithEval a) (arithEval b)
    | Not (a) -> (boolEval a) >>= (fun x -> ret (not x))
    | Conj (a, b) -> conj (boolEval a) (boolEval b)
    | IsLetter (a) ->
        (charEval a)
        >>= (fun x -> ret (System.Char.IsLetter x))
    | IsDigit (a) ->
        (charEval a)
        >>= (fun x -> ret (System.Char.IsDigit x))
    | IsVowel(_) -> failwith "Not Implemented"
    | IsConsonant(_) -> failwith "Not Implemented"


type stm =
    (* statements *)
    | Declare of string (* variable declaration *)
    | Ass of string * aExp (* variable assignment *)
    | Skip (* nop *)
    | Seq of stm * stm (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm (* while statement *)

let rec stmntEval stmnt : SM<unit> =
    match stmnt with
    | Declare x -> declare x
    | Ass (x, a) -> (arithEval a) >>= update x
    | Skip -> ret ()
    | Seq (s1, s2) -> (stmntEval s1) >>>= (stmntEval s2)
    | ITE (b, s1, s2) ->
        (boolEval b)
        >>= function
        | true -> push >>>= stmntEval s1 >>>= pop
        | false -> push >>>= stmntEval s2 >>>= pop
    | While (b, s1) ->
        (boolEval b)
        >>= function
        | true ->
            push
            >>>= stmntEval s1
            >>>= pop
            >>>= (stmntEval (While(b, s1)))
        | false -> ret ()

// (* Part 3 (Optional) *)

// type StateBuilder() =

//     member this.Bind(f, x) = f >>= x
//     member this.Return(x) = ret x
//     member this.ReturnFrom(x) = x
//     member this.Delay(f) = f ()
//     member this.Combine(a, b) = a >>= (fun _ -> b)

// let prog = new StateBuilder()

// let arithEval2 a = failwith "Not implemented"
// let charEval2 c = failwith "Not implemented"
// let rec boolEval2 b = failwith "Not implemented"

// let stmntEval2 stm = failwith "Not implemented"

(* Part 4 *)

type word = (char * int) list
type squareFun = word -> int -> int -> int
type coord = int * int
type boardFun = coord -> squareFun option

type board =
    { center: coord
      defaultSquare: squareFun
      squares: boardFun }

let createState w pos acc =
    mkState
        [ ("_pos_", pos)
          ("_acc_", acc)
          ("_result_", 0) ]
        w
        [ "_pos_"; "_acc_"; "_result_" ]

//refactor
let stmntToSquareFun =
    fun stm w pos acc ->
        evalSM (createState w pos acc) ((stmntEval stm) >>>= lookup ("_result_"))
        |> function
        | Failure _ -> 0
        | Success a -> a

let createBoardState x y =
    mkState
        [ ("_x_", x)
          ("_y_", y)
          ("_result_", 0) ]
        []
        [ "_x_"; "_y_"; "_result_" ]

let stmntToBoardFun =
    fun stm squares (x, y) ->
        (stmntEval stm) >>>= lookup ("_result_")
        >>= fun i ->
                match Map.tryFind i squares with
                | Some v -> ret (Some v)
                | _ -> ret (None)
        |> evalSM (createBoardState x y)
        |> fun res ->
            match res with
            | Failure _ -> None
            | Success a -> a
