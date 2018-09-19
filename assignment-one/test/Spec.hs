
import Lib
import Test.Hspec
import Test.QuickCheck




spec :: Spec
spec = do
  describe "Lowest Common Ancestor" $ do

    it "returns nothing for no input" $ do
      (lowestCommonAncestor ) 'shouldBe' Nothing

    it "returns nothing for only a tree input" $ do
      (lowestCommonAncestor tree ) 'shouldBe' Nothing

    it "returns itself for only one input" $ do
      (lowestCommonAncestor tree 5) == 5 'shouldBe' True

    it "returns 1 for inputs 2 and 3" $ do
      (lowestCommonAncestor tree 2 3) == 1 'shouldBe' True

    it "returns 1 if one of the inputs is 1 and the other is a child of 1" $ do
      (lowestCommonAncestor tree 1 2) == 1 'shouldBe' True
