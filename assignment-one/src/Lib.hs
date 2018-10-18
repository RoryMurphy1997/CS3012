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

a :: Path
a = [4,2,1] :# 3

b :: Path
b = [5,2,1] :# 3

someFunc :: IO ()
someFunc = do
  print (lcaToString (lca a b))

cons :: Id -> Path -> Path
cons a (ys :# n) = (a:ys) :# (n + 1)

lca :: Path -> Path -> Path
lca (xs0 :# i) (ys0 :# j) = if(xs0 == [] || ys0 == []) then empty else go k (drop (i-k) xs0) (drop (j-k) ys0) where
  k = min i j
  go n xxs@(x:xs) (y:ys) = if (x==y) then xxs :# n else go (n-1) xs ys

lcaToString :: Path -> [Int]
lcaToString ((x:xs) :# n) = (x:xs)
lcaToString empty = []
