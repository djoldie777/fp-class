module BST where

data BST a = Nil | Node (BST a) a (BST a)
	deriving (Show, Eq)

emptyBST :: (Ord a) => BST a -> Bool
emptyBST Nil = True
emptyBST _ = False

insertBST :: (Ord a) => BST a -> a -> BST a
insertBST Nil x = Node Nil x Nil
insertBST (Node l a r) x
  | x == a = Node l a r
  | x < a = Node (insertBST l x) a r
  | x > a = Node l a (insertBST r x)

containsBST :: (Ord a) => BST a -> a -> Bool
containsBST Nil _ = False
containsBST (Node l a r) x
  | x == a = True
  | x < a = containsBST l x
  | x > a = containsBST r x
