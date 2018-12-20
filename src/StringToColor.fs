module StringToColor 

let stringToColor (s : string) =
    match s with
    | "" -> "FFFFFF"
    | _ ->  let c = sprintf "%X" ((hash s) &&& 0x00FFFFFF)
            sprintf "%s%s" "00000".[0..5 - c.Length] c
