module Main where

data Tree t = Nil
    | Node t (Tree t) (Tree t)
    deriving (Show, Ord, Eq)


instance Functor Tree where
    fmap fn Nil = Nil
    fmap fn (Node v left Nil)    = Node (fn v) (fmap fn left) Nil
    fmap fn (Node v Nil right)   = Node (fn v)  Nil (fmap fn right)
    fmap fn (Node v left right)  = Node (fn v) (fmap fn left) (fmap fn right)


-- Returns size of a tree (in nodes count).
count :: (Ord a, Num a) => Tree a -> a
count Nil                 = 0
count (Node _ left right) = (+1) $ (+) (count left) (count right)


-- minVal and maxVal return smallest and largest nodes
-- in a tree.
maxVal :: (Ord a, Num a) => Tree a -> Tree a
maxVal Nil = Nil
maxVal (Node val left right)
            | (==) right Nil = (Node val left right)
            | otherwise      = maxVal right

minVal :: (Ord a, Num a) => Tree a -> Tree a
minVal Nil = Nil
minVal (Node val left right)
            | (==) left Nil = (Node val left right)
            | otherwise     = minVal left

-- Generates Tree from given list
fromList :: (Ord a, Num a) => [a] -> Tree a
fromList [] = Nil
fromList li = foldr insert Nil li

-- Generates list from given Tree (in sorted order).
toList :: (Ord a, Num a) => Tree a -> [a]
toList Nil                   = []
toList (Node val left right) = toList left ++ [val] ++ toList right

-- Add a new value to the tree.
insert :: (Ord a, Num a) => a -> Tree a -> Tree a
insert val Nil = Node val Nil Nil
insert val (Node v left right)
            | (<) v val   = rotate $ Node v left (insert val right)
            | otherwise   = rotate $ Node v (insert val left) right

-- Removes value from a tree.
remove :: (Ord a, Num a) => a -> Tree a -> Tree a
remove _ Nil = Nil
remove val (Node v left Nil)  | (==) val v = left
remove val (Node v Nil right) | (==) val v = right
remove val (Node v left right)
            | (<)  val v = rotate $ Node v (remove val left) right
            | (>)  val v = rotate $ Node v left (remove val  right)
            | otherwise  = rotate $ Node mRight left (remove mRight right)
            where mRight = value  $ minVal right

-- Returns existing node with searched value.
find :: (Ord a, Num a) => a -> Tree a -> Tree a
find val Nil = Nil
find val n@(Node v left right)
            | (<) val v = find val left 
            | (>) val v = find val right
            | otherwise = n


-- Utility function that is used to rebalance tree after 
-- manipulations that change its structure. It looks messy,
-- but at least covers all cases of right and left rotations.
rotate :: (Ord a, Num a) => Tree a -> Tree a
rotate Nil = Nil
rotate (Node val left right)
            | not $ isBalanced left  = Node val (rotate left)  right
            | not $ isBalanced right = Node val (rotate right) left
            | singleRR  = Node (value right) (Node val left (lSide right)) (rSide right)
            | singleRL  = Node (value left) (lSide left) (Node val (rSide left) right)
            | doubleRL  = Node (value $ lSide right) (Node val left (lSide (lSide right))) (Node (value right) (rSide $ lSide right) (rSide right))
            | doubleLR  = Node (value $ rSide left) (Node (value left) (lSide left) (lSide $ rSide left)) (Node val (rSide $ rSide left) right)
            | otherwise = Node val left right
            where singleRR = ((+1) . height $ left)  < (height right) && (height $ lSide right) < (height $ rSide right)
                  doubleRL = ((+1) . height $ left)  < (height right) && (height $ lSide right) > (height $ rSide right)
                  singleRL = ((+1) . height $ right) < (height left)  && (height $ rSide left)  < (height $ lSide left)
                  doubleLR = ((+1) . height $ right) < (height left)  && (height $ rSide left)  > (height $ lSide left)

height :: (Ord a, Num a) => Tree a -> a
height Nil = -1
height (Node _ left right) = (+1) maxHeight
             where maxHeight = max (height left) (height right)

balanceFactor :: (Ord a, Num a) => Tree a -> a
balanceFactor (Node _ left right) = abs $ (-) (height left) (height right)

isBalanced :: (Ord a, Num a) => Tree a -> Bool
isBalanced Nil = True
isBalanced (Node v left right)
            | (>1) $ balanceFactor (Node v left right) = False
            | not  $ isBalanced left                   = False
            | not  $ isBalanced right                  = False
            | otherwise                                = True


-- rSide and lSide return left or right side of a node.
rSide :: (Ord a, Num a) => Tree a -> Tree a
rSide Nil                 = Nil
rSide (Node _ left right) = right

lSide :: (Ord a, Num a) => Tree a -> Tree a
lSide Nil                 = Nil
lSide (Node _ left right) = left

-- Returns value of the node.
value :: (Ord a, Num a) => Tree a -> a
value Nil                   = 0
value (Node val left right) = val


main :: IO ()
main = print $ fromList [1..50]

