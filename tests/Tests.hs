module Tests where
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Vector3

v0 = Vector3 0 0 0
v1 = Vector3 5 5 5
v2 = Vector3 5.0 5.0 5.0
v3 = Vector3 1 1 1
v4 = Vector3 2 2 2
v5 = Vector3 (-1) (-1) (-1)

xUnit = Vector3 1 0 0
yUnit = Vector3 0 1 0
zUnit = Vector3 0 0 1

main :: IO ()
main = hspec $ do
  -- ** Vector3 Tests ** --
  
  -- Equality Tests
  describe "Vectors3 (Equality)" $ do
    it "should be equal to itself" $ do
      v0==v0 `shouldBe` True
    
    it "should be equal between ints and floats" $ do
      v1==v2 `shouldBe` True
      
    it "should be not equal if vectors are not equal" $ do
      v0==v1 `shouldBe` False

  -- Addition Tests
  describe "Vectors3 (Addition)" $ do
    it "should add the same vector" $ do
      (v3 `add` v3) `shouldBe` v4
      
    it "should add negative vectors" $ do
      (v3 `add` v5) `shouldBe` v0
  
  -- Subtraction Tests
  describe "Vector3 (Subtraction)" $ do
    it "should subtract the same vector" $ do
      (v3 `sub` v3) `shouldBe` v0
      
    it "should subtract negative vectors" $ do
      (v3 `sub` v5) `shouldBe` v4
  
  -- Negation Tests
  describe "Vector3 (Negation)" $ do
    it "should negate vector" $ do
      (neg v3) `shouldBe` v5