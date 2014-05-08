module ClosestPair where
import Data.List


maxPair :: (Int, Int)
maxPair = (maxBound, maxBound)

distance' :: [(Int, Int)] -> Float
distance' (x:y:[]) = distance x y
distance' (x:y:_)  = distance x y

distance :: (Int, Int) -> (Int, Int) -> Float
distance a b = sqrt $ fromIntegral $ ((fst a) - (fst b))^2 + ((snd a) - (snd b))^2

min_pair :: [(Int,Int)] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
min_pair (l1:l2:[]) (r1:r2:[]) []
    | (distance l1 l2) <= (distance r1 r2) = l1:[l2]
    | otherwise = r1:[r2]
min_pair (l1:l2:[]) (r1:r2:[]) (c1:c2:[])
    | (distance l1 l2) <= (distance r1 r2) && (distance l1 l2) <= (distance c1 c2) = l1:[l2]
    | (distance r1 r2) <= (distance l1 l2) && (distance r1 r2) <= (distance c1 c2) = r1:[r2]
    | otherwise = c1:[c2]
min_pair _ _ _ = []

closest_split_pair :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
closest_split_pair left [] min_pair'  = min_pair'
closest_split_pair [] right min_pair' = min_pair'
closest_split_pair (l:[]) (r:[]) min_pair' = min_pair (l:[r]) min_pair' []
closest_split_pair (l:ls) (r:rs) (m1:[m2])
    | (distance m1 m2) > pair_d = 
        min_pair (closest_split_pair ls (r:rs) (l:[r])) (closest_split_pair (l:ls) rs (l:[r])) (l:[r])
    | otherwise = 
        min_pair (closest_split_pair ls (r:rs) (m1:[m2])) (closest_split_pair (l:ls) rs (m1:[m2])) (l:[r])
    where
      pair_d = distance l r

closest_pair' :: [(Int, Int)] -> Int -> ([(Int, Int)], Int)
closest_pair' (x:y:[]) _   = ((x:[y]), 2)
closest_pair' (x:y:z:[]) _ = (min_pair (x:[y]) (y:[z]) (x:[z]), 2)
closest_pair' list length' = 
  let left_bound  = filter (\(x,y) -> mx - d <= x && x <= mx + d) left
      right_bound = filter (\(x,y) -> mx - d <= x && x <= mx + d) right in
    let split' = closest_split_pair left_bound right_bound (min_pair left' right' []) in
      (min_pair left' right' split', mid) 
  where
    mid   = length' `div` 2
    left  = take mid list
    right = drop mid list
    ((mx,_):ms) = right
    (left', _)  = closest_pair' left mid
    (right', _) = closest_pair' right mid
    d_left  = distance' left
    d_right = distance' right
    d       = ceiling $ min d_left d_right

closest_pair :: [(Int, Int)] -> [(Int, Int)]
closest_pair list 
    | odd length' = 
        let (pair, cnt) = closest_pair' (sort $ maxPair:list) (length' + 1) in pair
    | otherwise   = 
        let (pair, cnt) = closest_pair' (sort list) length' in pair
  where
    length' = length list
