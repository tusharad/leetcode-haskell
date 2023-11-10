module LinkedList.MergeTwoLists where
import LinkedList.LinkedList

mergeTwoLists :: SinglyLinkedList -> SinglyLinkedList -> SinglyLinkedList
mergeTwoLists list1 list2 = next $ mergeTwoLists_ list1 list2 (SinglyLinkedList {val = 0,next = Nil})
    where
        mergeTwoLists_ Nil list2 res = res { next = list2 }
        mergeTwoLists_ list1 Nil res = res { next = list1 }
        mergeTwoLists_ list1 list2 res
            | (val list1) <= (val list2) = mergeTwoLists_ (next list1) list2 (res {next = list1})
            |  otherwise = mergeTwoLists_ list1 (next list2) (res {next = list2})
