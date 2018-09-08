module Hopti (
    vns
) where

argmax :: (Ord o) => (a->o) -> [a] -> a
argmax ob [x] = x
argmax ob (x:xs)
    | ob x >= ob x' = x
    | otherwise     = x'
    where x' = argmax ob xs

-- Variant Neighborhood Search
vns :: (Eq s, Real o) => (s->o) -> [s->[s]] -> s -> s
vns ob [] s = s
vns ob (mv:mvs) s
    | s==s'     = vns ob mvs s
    | otherwise = vns ob (mv : mvs) s'
    where s' = argmax ob (s : mv s)

-- #####################
-- Example: Knapsack problem:
-- #####################

-- Move of adding a weight:
move_add :: [Int] -> [Int] -> [[Int]]
move_add ws s = map (\w -> w:s) ws

-- Move of droping a weight:
move_drop :: [Int] -> [[Int]]
move_drop [] = []
move_drop (e:s) = s : map (e:) (move_drop s)

-- Evaluate solution:
target :: Int -> [Int] -> Int
target mx s = let v = sum s in if v > mx then -1 else v

-- Solve Knapsack problem, start from empty solution:
solve_knapsack :: [Int] -> Int -> [Int]
solve_knapsack ws mx = vns (target mx) [move_add ws,move_drop] []
