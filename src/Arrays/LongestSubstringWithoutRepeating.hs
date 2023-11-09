module Arrays.LongestSubstringWithoutRepeating where
import qualified Data.Set as St
 
deleteFromSet :: String -> Int -> Int -> St.Set Char -> (St.Set Char,Int)
deleteFromSet s i j set
    | (St.member (s !! j) set) && i < j = deleteFromSet s (i+1) j (St.delete (s !! i) set)
    | otherwise = (set,i)

lengthOfLongestSubstring_ :: String -> Int -> Int -> Int -> St.Set Char -> Int
lengthOfLongestSubstring_ s i j res set
    | j == length s = res
    | otherwise = do
        let (set',i') = deleteFromSet s i j set
        lengthOfLongestSubstring_ s i' (j+1) (max res (j-i'+1)) (St.insert (s !! j) set')

lengthOfLongestSubstring :: String -> Int
lengthOfLongestSubstring s = lengthOfLongestSubstring_ s 0 0 0 St.empty