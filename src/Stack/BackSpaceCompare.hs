module Stack.BackSpaceCompare where

-- https://leetcode.com/problems/backspace-string-compare/description/
removeBackSpace :: String -> String -> String
removeBackSpace "" res = res
removeBackSpace ['#'] res = res
removeBackSpace ('#':_:xs) res = removeBackSpace xs res
removeBackSpace (x:xs) res = removeBackSpace xs (x:res)

backSpaceCompare :: String -> String -> Bool
backSpaceCompare s t = removeBackSpace (reverse s) "" == removeBackSpace (reverse t) ""