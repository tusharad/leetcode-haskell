module Arrays.LongestCommonPrefix where
import Data.List

longestCommonPrefix :: [String] -> String
longestCommonPrefix xs = [x | (x,y) <- zip (head $ sort xs) (last $ sort xs), x==y]
