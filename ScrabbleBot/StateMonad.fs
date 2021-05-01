// Insert your StateMonad.fs from Assignment 6 here. All modules must be internal.


module internal StateMonad

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string           

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)


    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> =
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    let pop : SM<unit> =
        S (fun s -> Success ((), {s with vars =  s.vars.Tail }))

    let wordLength : SM<int> =  S (fun s -> Success (s.word.Length, s ))

    let characterValue (pos : int) : SM<char> = 
      S (fun s ->
            match pos with
            | pos when pos >= s.word.Length || pos < 0 -> Failure(IndexOutOfBounds pos)
            | _ -> Success (fst(s.word.[pos]), s)
            // if (pos >= s.word.Length || pos < 0 ) then Failure(IndexOutOfBounds pos)
            // else Success (fst(s.word.[pos]), s)
      )

    let pointValue (pos : int) : SM<int> = 
        S (fun s ->
                match pos with
                | pos when pos >= s.word.Length || pos < 0 -> Failure(IndexOutOfBounds pos)
                | _ -> Success (snd(s.word.[pos]), s)
                // if (pos >= s.word.Length || pos < 0 ) then Failure(IndexOutOfBounds pos)
                // else Success (snd(s.word.[pos]), s)
            )    

    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
              match aux (s.vars) with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))

    let update (var : string) (value : int) : SM<unit> =
        let rec aux =
            function
            | m :: ms ->
                if (Map.containsKey var m) then Some (m.Add(var, value) :: ms)
                else
                    match aux ms with
                    | None -> None
                    | Some ms' -> Some (m :: ms')
            | [] -> None

        S (fun s ->
            match aux (s.vars) with
            | Some list -> Success ((), {s with vars =  list })
            | None  -> Failure (VarNotFound var))



    let declare (var : string) : SM<unit> =
        S ( fun s ->
            match s with
            | s when s.reserved.Contains(var) -> Failure (ReservedName var)
            | s when s.vars.Head.ContainsKey(var) -> Failure (VarExists var)
            | s when s.vars.IsEmpty -> Success((), {s with vars = s.vars.Head.Add(var, 0) ::[]})
            | s -> Success(() , { s with vars = s.vars.Head.Add(var, 0) :: s.vars.Tail})
        )
