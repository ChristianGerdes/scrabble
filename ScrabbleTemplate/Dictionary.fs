// Insert your Dictionary.fsi file here. All modules must be internal.

module internal Dictionary

    type Dict = Temp of unit // Not implemented

    // If you have made a lookup function you may include it here, but it will be generated for you by the project.

    let empty : unit -> Dict = fun _ -> failwith "Not implemented"
    let insert : string -> Dict -> Dict = fun _ _ -> failwith "Not implemented"
    let step : char -> Dict -> (bool * Dict) option = fun _ _ -> failwith "Not implemented"


    // Only implement reverse if you have made a Gaddag
    let reverse : Dict -> (bool * Dict) option = fun _ -> failwith "Not implemented"