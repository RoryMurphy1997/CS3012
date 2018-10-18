module Lib
    ( lca,
      Path(..),
      someFunc,
      Id,
      lcaToString
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
  print (lcaToString (lca a b))
  --print (lcaToString (lca a f))
  print (lcaToString (lca d e))
  print (lcaToString (lca b b))
  print (lcaToString (lca c d))
  print (lcaToString (lca a g))
  print (lcaToString (lca a c))
  print (lcaToString (lca f h))

cons :: Id -> Path -> Path
cons a (ys :# n) = (a:ys) :# (n + 1)

lca :: Path -> Path -> Path
lca (xs0 :# i) (ys0 :# j) = if(xs0 == [] || ys0 == []) then empty else go k (drop (i-k) xs0) (drop (j-k) ys0) where
  k = min i j
  go n xxs@(x:xs) (y:ys) = if (x==y) then xxs :# n else go (n-1) xs ys

pathToArray :: Path -> [Int]
pathToArray ((x:xs) :# n) = (x:xs)
pathToArray empty = []
