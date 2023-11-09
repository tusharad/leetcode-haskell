module LinkedList.AddTwoNumbers where
import LinkedList.LinkedList as LL ( SinglyLinkedList(..) )
-- https://leetcode.com/problems/add-two-numbers/

addTwoNumbers_ :: SinglyLinkedList -> SinglyLinkedList -> Int -> SinglyLinkedList
addTwoNumbers_ x y carry
    | x /= Nil && y /= Nil = SinglyLinkedList {val = (val x + val y + carry) `mod` 10, next = addTwoNumbers_ (next x) (next y) ((val x + val y + carry) `div` 10)} 
    | x /= Nil = SinglyLinkedList {val = (val x + carry) `mod` 10, next = addTwoNumbers_ (next x) Nil ((val x + carry) `div` 10)} 
    | y /= Nil = SinglyLinkedList {val = (val y + carry) `mod` 10, next = addTwoNumbers_ Nil (next y) ((val y + carry) `div` 10)} 
    | carry /= 0 = SinglyLinkedList {val = carry,next = Nil}
    | otherwise = Nil

addTwoNumbers :: SinglyLinkedList -> SinglyLinkedList -> SinglyLinkedList
addTwoNumbers x y = next SinglyLinkedList {val = 0,next = addTwoNumbers_ x y 0}
