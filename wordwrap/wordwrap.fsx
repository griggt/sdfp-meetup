
let rec indexWhere (pred: char -> bool) pos (str: string) =
    match max 0 pos with
      | i when i < str.Length ->
        if pred str.[i] then
          Some i
        else
          indexWhere pred (i+1) str
      | _ -> None

let rec lastIndexWhere (pred: char -> bool) pos (str: string) =
    match min pos str.Length with
      | i when i >= 0 ->
        if pred str.[i] then
          Some i
        else
          lastIndexWhere pred (i-1) str
      | _ -> None

let rec wrap col (str: string) =
    let breakAt pos =
        match pos with
          | None -> str
          | Some p -> str.[..p] + "\n" + (str.[p+1..] |> wrap col)

    let breakPos =
        match str.Length with
          | n when n <= col -> None
          | _ ->
            let pos = str |> lastIndexWhere System.Char.IsWhiteSpace col
            match pos with
              | None -> str |> indexWhere System.Char.IsWhiteSpace (col+1)
              | _ -> pos

    breakAt breakPos
