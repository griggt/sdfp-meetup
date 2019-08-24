type Roll = Int

data Frame =
    Strike
    | Spare Roll
    | Missed Roll Roll

scoreFrame :: Frame -> [Roll] -> (Int, [Roll])
scoreFrame frame rolls =
    let (numRolls, numBonusRolls, baseScore) =
            case frame of Strike -> (1, 2, 10)
                          Spare _ -> (2, 1, 10)
                          Missed r1 r2 -> (2, 0, r1 + r2)
        futureRolls = drop numRolls rolls
        bonusScore = sum $ take numBonusRolls futureRolls
    in (baseScore + bonusScore, futureRolls)

scoreRecursive :: [Frame] -> [Roll] -> Int -> Int
scoreRecursive [] _ acc = acc
scoreRecursive (f:fs) rolls acc =
    let (frameScore, futureRolls) = scoreFrame f rolls
    in  scoreRecursive fs futureRolls (acc + frameScore)

score :: [Frame] -> [Roll] -> Int
score frames bonus =
    let frameRolls = frames >>= \f ->
            case f of
                Strike -> [10]
                Spare r1 -> [r1, 10 - r1]
                Missed r1 r2 -> [r1, r2]
        allRolls = frameRolls ++ bonus
    in  scoreRecursive frames allRolls 0

-- testing
s1 = score (replicate 10 Strike) [10, 10]
s2 = score (replicate 10 (Spare 5)) [5]
s3 = score (replicate 10 (Missed 9 0)) []
