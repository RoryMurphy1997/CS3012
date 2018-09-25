module Spec where

import Lib
import Test.Hspec
import Test.QuickCheck

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

myTree :: Tree Int
myTree = Node 1
    (Node 2
        (Node 4 Empty Empty)
        (Node 5 Empty Empty))
    (Node 3
        (Node 6 Empty Empty)
        (Node 7 Empty Empty))

spec :: Spec
spec = do
  describe "Lowest Common Ancestor" $ do

    it "returns nothing for no input" $ do
      (lca ) `shouldBe` Nothing

    it "returns nothing for only a tree input" $ do
      (lca myTree ) `shouldBe` Nothing

    it "returns itself for only one input" $ do
      (lca myTree 5) == 5 `shouldBe` True

    it "returns 1 for inputs 2 and 3" $ do
      (lca myTree 2 3) == 1 `shouldBe` True

    it "returns 1 if one of the inputs is 1 and the other is a child of 1" $ do
      (lca myTree 1 2) == 1 `shouldBe` True
