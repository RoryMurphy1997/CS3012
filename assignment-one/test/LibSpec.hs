module LibSpec where

import           Lib
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "Lowest Common Ancestor" $ do

    it "returns empty if passed an Empty tree" $
      lca empty empty == [] :# 0 `shouldBe` True

    it "returns empty if passed two nodes not in same graph" $
      lca a f == [] :# 0 `shouldBe` True

    it "returns the node if the same node is input twice" $
      lca b b == [5,2,1] :# 3 `shouldBe` True

    it "returns lowest common ancestor for two different paths" $
      lca c d == [2,1] :# 2 `shouldBe` True

    it "returns full item 2 if path 1 is path 2's direct ancestor" $
      lca a g == [2,1] :# 2 `shouldBe` True

    it "returns path 2 levels up if two given paths differe by 2 items" $
      lca a c == [1] :# 1 `shouldBe` True

    it "returns single lowest common ancestor when more than one ancestor exists" $
      lca c e == Right 1 `shouldBe` True

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
e = [6,2,1] :# 3

f :: Path
f = [11,12,13,14] :# 4

g :: Path
g = [2,1] :# 2
