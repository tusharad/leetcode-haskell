module Arrays.ContainerArea where
import Data.Vector.Unboxed as V
-- https://leetcode.com/problems/container-with-most-water/

maxArea_ :: V.Vector Int -> Int -> Int -> Int -> Int
maxArea_ vec left right res
    | left >= right = res
    | vec V.! left < vec V.! right = maxArea_ vec (left+1) right (max res ((vec V.! left)*(right-left)))
    | otherwise = maxArea_ vec left (right-1) (max res ((vec V.! right)*(right-left)))

maxArea :: V.Vector Int -> Int
maxArea vec = maxArea_ vec 0 (V.length vec - 1) 0