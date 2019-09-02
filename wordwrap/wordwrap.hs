import Data.Char
import Data.List

findBreakPosition :: [Char] -> Int -> Int
findBreakPosition [] col = 0
findBreakPosition str col
    | length str <= col = -1
    | otherwise =
        let breaks = takeWhile (<= col) $ findIndices isSpace str
            trivial = last breaks  -- TODO may be empty list, pattern match?
        in trivial
    -- TODO
    -- If length of str <= col, return -1 or other sentinel?
    -- Otherwise find last index of whitespace at of before cols

doBreak :: [Char] -> Int -> [Char]
doBreak str (-1) = str
doBreak str pos =
    let parts = splitAt pos str
    in (fst parts) ++ "\n" ++ wrap (drop 1 $ snd parts) pos

wrap :: [Char] -> Int -> [Char]
wrap str col = doBreak str $ findBreakPosition str col
