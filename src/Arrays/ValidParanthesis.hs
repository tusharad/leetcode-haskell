module Arrays.ValidParanthesis where
    
-- https://leetcode.com/problems/valid-parentheses/
isValid_ :: String -> String -> Bool
isValid_ [] [] = True
isValid_ [] _ = False
isValid_ (x:xs) st
    | (x == '(') || (x == '{') || (x == '[') = isValid_ xs (x:st)
    | st == [] = False
    | (x == '}') = if (head st) == '{' then isValid_ xs (tail st) else False
    | (x == ']') = if (head st) == '[' then isValid_ xs (tail st) else False
    | (x == ')') = if (head st) == '(' then isValid_ xs (tail st) else False
    

isValid :: String -> Bool
isValid str = isValid_ str []