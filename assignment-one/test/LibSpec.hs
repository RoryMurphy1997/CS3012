module LibSpec where

import           Lib
import           Test.Hspec
import           Test.QuickCheck

-- data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

spec :: Spec
spec = do
  describe "Lowest Common Ancestor" $ do

    it "returns the node if the same node is input twice" $
      lca myTree 5 5 == Right 5 `shouldBe` True

    it "returns parent for input of its two children" $
      lca myTree 2 3 == Right 1 `shouldBe` True

    it "returns parent if one of the inputs is parent and the other is a left child" $
      lca myTree 3 6 == Right 3 `shouldBe` True

    it "returns parent if one of the inputs is parent and the other is a right child" $
      lca myTree 2 5 == Right 2 `shouldBe` True

    it "returns grandparent for input of two of its grandchildren" $
      lca myTree 4 7 == Right 1 `shouldBe` True


myTree :: Tree Integer
myTree =  Node 1
         (Node 2
             (Node 4 Empty Empty)
             (Node 5 Empty Empty))
         (Node 3
             (Node 6 Empty Empty)
             (Node 7 Empty Empty))
