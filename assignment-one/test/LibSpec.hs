module LibSpec where

import           Lib
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "Lowest Common Ancestor" $ do

    it "returns empty if passed an Empty tree" $
      pathToArray(lca empty empty) == [] `shouldBe` True

    it "returns empty if passed two nodes not in same graph" $
      pathToArray(lca a f) == [] `shouldBe` True

    it "returns nothing even if connected at end" $
      pathToArray(lca d e) == [] `shouldBe` True

    it "returns the node if the same node is input twice" $
      pathToArray(lca b b) == pathToArray(b) `shouldBe` True

    it "returns lowest common ancestor for two different paths" $
      pathToArray(lca c d) == pathToArray(g) `shouldBe` True

    it "returns full item 2 if path 1 is path 2's direct ancestor" $
      pathToArray(lca a g) == pathToArray(g) `shouldBe` True

    it "returns path 2 levels up if two given paths differe by 2 items" $
      pathToArray(lca a c) == pathToArray(h) `shouldBe` True

    it "returns lowest common ancestor even when paths are joined further down" $
      pathToArray(lca f h) == pathToArray(i) `shouldBe` True




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

i :: Path
i = [13,14] :# 2
