module Arrays.RemoveDuplicates where
import Data.Vector.Unboxed as V

removeDuplicates_ :: V.Vector Int -> Int -> Int -> (V.Vector Int,Int)
removeDuplicates_ nums i low
    | i == V.length nums  = (nums,low+1)
    | otherwise = do 
        if nums V.! i /= nums V.! low then removeDuplicates_ (nums V.// [(low+1,nums V.! i)]) (i+1) (low+1)        
        else removeDuplicates_ nums (i+1) low

removeDuplicates :: V.Vector Int -> (V.Vector Int,Int)
removeDuplicates nums = removeDuplicates_ nums 0 0