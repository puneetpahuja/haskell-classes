-- bst.hs

module BST where

data BST = Nil | Node BST Int BST deriving (Show, Eq)

myBst :: BST
myBst = Node
          (Node
            (Node Nil 10 Nil)
            20
            (Node Nil 40 Nil))
          50
          (Node
            (Node Nil 60 Nil)
            70
            (Node Nil 90 Nil))

find :: Int -> BST -> Bool
find element bst = case bst of
  Nil -> False
  (Node left element' right) -> (element' == element) || if element' < element
                                                         then find element right
                                                         else find element left

insert :: Int -> BST -> BST
insert element bst = case bst of
  Nil -> Node Nil element Nil
  (Node left element' right) -> if element' == element
                              then Node left element' right
                              else if element' < element
                                      then Node left element' (insert element right)
                                      else Node (insert element left) element' right


delete :: Int -> BST -> BST
delete _ Nil = Nil
delete element (Node left element' right) | element' < element = Node left element' (delete element right)
                                          | element' > element = Node (delete element left) element' right
                                          | left == Nil        = right
                                          | right == Nil       = left
                                          | otherwise          = Node left (minElement right) (delete (minElement right) right)

minElement :: BST -> Int
minElement bst = case bst of
  (Node Nil element Nil) -> element
  (Node left _ _)  -> minElement left

-- assumes root is at level 0
depth :: BST -> Int
depth Nil = -1
depth (Node left _ right) = 1 + (max (depth left) (depth right))


{-
myBst
Node (Node (Node Nil 10 Nil) 20 (Node Nil 40 Nil)) 50 (Node (Node Nil 60 Nil) 70 (Node Nil 90 Nil))

find 50 myBst
True

find 60 myBst
True

find 80 myBst
False

insert 80 (insert 55 (insert 75 myBst))
Node (Node (Node Nil 10 Nil) 20 (Node Nil 40 Nil)) 50 (Node (Node (Node Nil 55 Nil) 60 Nil) 70 (Node (Node Nil 75 (Node Nil 80 Nil)) 90 Nil))

find 55 myBst
False

find 55 (insert 80 (insert 55 (insert 75 myBst)))
True



delete 1 myBst
Node (Node (Node Nil 10 Nil) 20 (Node Nil 40 Nil)) 50 (Node (Node Nil 60 Nil) 70 (Node Nil 90 Nil))

delete 10 myBst
Node (Node Nil 20 (Node Nil 40 Nil)) 50 (Node (Node Nil 60 Nil) 70 (Node Nil 90 Nil))

delete 20 (delete 10 myBst)
Node (Node Nil 40 Nil) 50 (Node (Node Nil 60 Nil) 70 (Node Nil 90 Nil))

delete 20 (delete 400 myBst)
Node (Node (Node Nil 10 Nil) 40 Nil) 50 (Node (Node Nil 60 Nil) 70 (Node Nil 90 Nil))

delete 20 (delete 40 myBst)
Node (Node Nil 10 Nil) 50 (Node (Node Nil 60 Nil) 70 (Node Nil 90 Nil))

delete 50 myBst
Node (Node (Node Nil 10 Nil) 20 (Node Nil 40 Nil)) 60 (Node Nil 70 (Node Nil 90 Nil))

delete 10 (delete 40 (delete 60 (delete 90 myBst)))
Node (Node Nil 20 Nil) 50 (Node Nil 70 Nil)

delete 50 (delete 10 (delete 40 (delete 60 (delete 90 myBst))))
Node (Node Nil 20 Nil) 70 Nil

depth myBst
2

depth (delete 50 (delete 10 (delete 40 (delete 60 (delete 90 myBst)))))
1

depth (delete 20 (delete 70 (delete 50 (delete 10 (delete 40 (delete 60 (delete 90 myBst)))))))
-1

depth (delete 70 (delete 50 (delete 10 (delete 40 (delete 60 (delete 90 myBst))))))
0
-}
