module MyLib (someFunc) where
import Arrays.TwoSum
import LinkedList.MergeTwoLists
import LinkedList.LinkedList

someFunc :: IO ()
someFunc = do
    print $ mergeTwoLists (createList [1,2,8]) (createList [4,5,10])
    putStrLn "someFunc"
