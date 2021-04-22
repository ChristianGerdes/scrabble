// Insert your Dictionary.fsi file here. All modules must be internal.

module internal Dictionary
    type Dict = | D of Map<char, bool * Dict>

    // If you have made a lookup function you may include it here, but it will be generated for you by the project.

    let empty : unit -> Dict = fun () -> D (Map.empty)


    // let insert : string -> Dict -> Dict = fun s (D d) -> D (d.Add(s))
    let insert



    let step : char -> Dict -> (bool * Dict) option = fun _ _ -> failwith "Not implemented"


    // Only implement reverse if you have made a Gaddag
    let reverse : Dict -> (bool * Dict) option = fun _ -> failwith "Not implemented"




// module Dict
//    type Dict= | D of Set<string>

//    let empty = fun () -> D (Set.empty)

//    let insert s (D d) =  D (d.Add(s))

//    let lookup s (D d) =  d.Contains(s)

