module Arrays.ThreeSumClosest where
import Data.List

updateC :: Int -> Int -> Int -> Int
updateC closest target total
    | (abs (target-total)) < (abs (target-closest)) = total
    | otherwise = closest

findClosest :: [Int] -> Int -> Int -> Int -> Int -> Int -> Int -> Int
findClosest nums first second third target closest result
    | second >= third = result
    | result /= (-1) = result
    | otherwise = do
        let total = ((nums !! first) + (nums !! second) + (nums !! third))
        if total == target then total
        else if (total < target) then findClosest nums first (second+1) third target (updateC closest target total) result
        else findClosest nums first second (third-1) target (updateC closest target total) result


threeSumClosest_ :: [Int] -> Int -> Int -> Int -> Int -> Int
threeSumClosest_ nums target closest first result
    | first == (length nums - 2) = result
    | result /= (-1) = result
    | otherwise = threeSumClosest_ nums target closest (first+1) (findClosest nums first (first+1) (length nums - 1) target closest result)

threeSumClosest :: [Int] -> Int -> Int
threeSumClosest nums target = threeSumClosest_ (sort nums) target ((nums !! 0) + (nums !! 1) + (nums !! 2)) 0 (-1)
