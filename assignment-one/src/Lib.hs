module Lib
    ( cons,
      lca,
      ID,
      Path,
      someFunc
    ) where

someFunc :: IO ()
someFunc = do
  putStrLn (lca a b)

type Id = Int
data Path = [Id] :# !Int

empty :: Path
empty = [] :# 0

--Paths a,b,c,d make up BST from previous solution
a :: Path
a = [4,2,1] :# 3

b :: Path
b = [5,2,1] :# 3

cons :: Id -> Path -> Path
cons a (ys :# n) = (a:ys) :# (n + 1)

lca :: Path -> Path -> Path
lca (xs0 :# i) (ys0 :# j) = go k (drop (i-k) xs0) (drop (j-k) ys0) where
  k = min i j
  go !n xxs@(x:xs) (y:ys)
    | x == y   = xxs :# n
    | otherwse = go (n - 1) xs ys
