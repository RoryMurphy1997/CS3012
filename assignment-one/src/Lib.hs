module Lib
    ( lca,
      Path(..),
      someFunc,
      Id,
      lcaOfPath
    ) where


type Id = Int
data Path = [Id] :# !Int

empty :: Path
empty = [] :# 0
--Paths a,b,c,d make up BST from previous solution
a :: Path
a = [4,2,1] :# 3

b :: Path
b = [5,2,1] :# 3

c :: Path
c = [6,3,1] :# 3

d :: Path
d = [7,3,1] :# 3

e :: Path
e = [7,8,9] :# 3

f :: Path
f = [11,12,13,14] :# 4

g :: Path
g = [2,1] :# 2

h :: Path
h = [11,15,13,14] :# 4


someFunc :: IO ()
someFunc = do
  print (lcaOfPath (lca a b))
  print (lcaOfPath (lca a f))
  print (lcaOfPath (lca d e))
  print (lcaOfPath(lca b b))
  print (lcaOfPath (lca c d))
  print (lcaOfPath (lca a g))
  print (lcaOfPath (lca a c))
  print (lcaOfPath (lca f h))

cons :: Id -> Path -> Path
cons a (ys :# n) = (a:ys) :# (n + 1)

lca :: Path -> Path -> Path
lca (xs0 :# i) (ys0 :# j) = if(xs0 == [] || ys0 == [] || compareList xs0 ys0 == False) then empty else go k (drop (i-k) xs0) (drop (j-k) ys0) where
  k = min i j
  go n xxs@(x:xs) (y:ys) = if (x==y) then xxs :# n else go (n-1) xs ys

lcaOfPath :: Path -> Int  --Returns the lowest integer of the path of the lowest common ancestor
lcaOfPath ((x:xs) :# n) = x
lcaOfPath empty = -1     --Returns -1 as a null value

compareList :: (Eq a) => [a] -> [a] -> Bool
compareList [] _ = False
compareList (x:xs) ys
    | elem x ys = True
    | otherwise  = compareList xs ys
