substituir :: Int -> Int -> [Int] -> [Int]
substituir _ _ [] = []
substituir x y (a:l)
    | x == a    = [y] ++ substituir x y l
    | otherwise = [a] ++ substituir x y l