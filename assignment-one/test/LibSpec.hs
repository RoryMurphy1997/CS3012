module LibSpec where

import           Lib
import           Test.Hspec
import           Test.QuickCheck


spec :: Spec
spec = do
  describe "Lowest Common Ancestor" $ do

    it "returns Left False if passed an Empty tree" $
      lca Empty 5 5 == Left False `shouldBe` True

    it "returns Left False if passed two nodes not in tree" $
      lca myTree 0 9 == Left False `shouldBe` True

    it "returns Left True if passed one node not in tree even if the other is (Left True being sign that one of the nodes has been found)" $
      lca myTree 0 5 == Left True `shouldBe` True

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
