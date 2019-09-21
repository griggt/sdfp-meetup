type Roll = Roll of int

type Frame =
    | Strike
    | Spare of Roll
    | Missed of Roll * Roll

let pins (Roll r) = r

let scoreFrame frame rolls =
    let numRolls, numBonusRolls, baseScore =
      match frame with
        | Strike -> (1, 2, 10)
        | Spare _ -> (2, 1, 10)
        | Missed (r1, r2) -> (2, 0, (r1 |> pins) + (r2 |> pins))

    let futureRolls = rolls |> List.skip numRolls
    let bonusScore = futureRolls |> List.take numBonusRolls |> List.sumBy pins
    (baseScore + bonusScore, futureRolls)

let rec scoreRecursive remainingFrames rolls acc =
    match remainingFrames with
      | [] -> acc
      | f::fs ->
          let frameScore, futureRolls = scoreFrame f rolls
          scoreRecursive fs futureRolls (acc + frameScore)

let score frames bonus =
    let frameRolls = frames |> List.collect (fun f ->
        match f with
          | Strike -> [Roll 10]
          | Spare r1 -> [r1; Roll (10 -  (r1 |> pins))]
          | Missed (r1, r2) -> [r1; r2]
    )
    let allRolls = frameRolls @ bonus
    scoreRecursive frames allRolls 0

//// Testing

let f1 = List.init 10 (fun _ -> Strike)
let b1 = [Roll 10; Roll 10]
let s1 = score f1 b1

let f2 = List.init 10 (fun _ -> Spare (Roll 5))
let b2 = [Roll 5]
let s2 = score f2 b2

let f3 = List.init 10 (fun _ -> Missed (Roll 9, Roll 0))
let b3 = []
let s3 = score f3 b3
