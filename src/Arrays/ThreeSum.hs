module Arrays.ThreeSum where
import Data.List

nextRight :: Int -> Int -> Int -> [Int] -> Int
nextRight right left currentRight lst 
    | right > left && currentRight == (lst !! right) = nextRight (right-1) left currentRight lst
    | otherwise = right

nextLeft :: Int -> Int -> Int -> [Int] -> Int
nextLeft left right currentLeft lst
    | left < right && currentLeft == (lst !! left) = nextLeft (left+1) right currentLeft lst
    | otherwise = left

findPair :: [Int] -> Int -> Int -> Int -> [[Int]] -> [[Int]]
findPair lst index left right res
    | left >= right = res
    | otherwise = if (((lst !! index) + (lst !! left) + (lst !! right)) == 0) then findPair lst index (nextLeft left right (lst !! left) lst) (nextRight right left (lst !! right) lst) (res ++ [[(lst !! index),(lst !! left),(lst !! right)]])
        else if (((lst !! index) + (lst !! left) + (lst !! right)) > 0) then findPair lst index left (right-1) res
        else findPair lst index (left+1) right res 

threeSum_ :: [Int] -> Int -> Int -> Int -> [[Int]] -> [[Int]]
threeSum_ lst index left right res 
    | index == (length lst - 1) = res
    | index > 0 && (lst !! (index-1)) == (lst !! index) = threeSum_ lst (index+1) (left+1) right res
    | otherwise = threeSum_ lst (index+1) (index+2) right ((findPair lst index left right res))

threeSum :: [Int] -> [[Int]]
threeSum lst = threeSum_ (sort lst) 0 1 (length lst -1) []
