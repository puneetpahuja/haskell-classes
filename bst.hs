-- bst.hs

module BST where

data BST = Nil | Node BST Int BST deriving Show

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
  (Node left val right) -> (val == element) || if val < element
                                                  then find element right
                                                  else find element left

insert :: Int -> BST -> BST
insert element bst = case bst of
  Nil -> Node Nil element Nil
  (Node left val right) -> if val == element
                              then Node left val right
                              else if val < element
                                      then Node left val (insert element right)
                                      else Node (insert element left) val right


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
-}
