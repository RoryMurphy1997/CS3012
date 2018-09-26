module Spec where

import Lib
import Test.Hspec
import Test.QuickCheck

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show



spec :: IO ()
spec = hspec $ do
  describe "Lowest Common Ancestor" $ do

    it "returns nothing for no input" $
      lca_show  `shouldBe` Nothing

    it "returns nothing for only a tree input" $
      lca_show myTree  `shouldBe` Nothing

    it "returns itself for only one input" $
      lca_show myTree 5 == 5 `shouldBe` True

    it "returns parent for input of its two children" $
      lca_show myTree 2 3 == 1 `shouldBe` True

    it "returns parent if one of the inputs is parent and the other is a child" $
      lca_show myTree 3 4 == 3 `shouldBe` True

    it "returns grandparent for input of two of its grandchildren" $
      lca_show myTree 4 7 == 1 `shouldBe` True


    myTree :: Tree Int
    myTree = Node 1
        (Node 2
            (Node 4 Empty Empty)
            (Node 5 Empty Empty))
        (Node 3
            (Node 6 Empty Empty)
            (Node 7 Empty Empty))
