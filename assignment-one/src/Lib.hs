module Lib
    ( empty,
      cons,
      lca,
      Path
    ) where

someFunc :: IO ()
someFunc = do
  putStrLn ( lca_show myTree' 0 9)
  putStrLn ( lca_show myTree' 0 5)
  putStrLn ( lca_show myTree' 4 5)
  putStrLn ( lca_show myTree' 4 6)
  putStrLn ( lca_show myTree' 3 4)
  putStrLn ( lca_show myTree' 2 4)
  putStrLn ( lca_show myTree' 5 5)

type Id = Int
data Path = [Id] :# !Int

empty :: Path
empty = [] :# 0

cons :: Id -> Path -> Path
cons a (ys :# n) = (a:ys) :# (n + 1)

lca :: Path -> Path -> Path
lca (xs0 :# i) (ys0 :# j) = go k (drop (i-k) xs0) (drop (j-k) ys0) where
  k = min i j
  go !n xxs@(x:xs) (y:ys)
    | x == y   = xxs :# n
    | otherwse = go (n - 1) xs ys
