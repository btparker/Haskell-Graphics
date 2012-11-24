module Tests where
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Vector3

xUnit = Vector3 1 0 0
yUnit = Vector3 0 1 0
zUnit = Vector3 0 0 1

v0 = Vector3 0 0 0
v1 = Vector3 5 5 5
v2 = Vector3 5.0 5.0 5.0
v3 = Vector3 1 1 1
v4 = Vector3 2 2 2
v5 = Vector3 (-1) (-1) (-1)
v6 = Vector3 3 (-4) (-1)
v7 = Vector3 0 5 2
v8 = Vector3 2 1 (-1)
v9 = Vector3 (-3) 4 1
v10 = Vector3 5 1 11
v11 = Vector3 (-5) (-1) (-11)

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
      
    it "should negate (negative) vector" $ do
      (neg v5) `shouldBe` v3
  
  -- Multiplication Tests
  describe "Vector3 (Multiplication)" $ do
    it "should multiply by positive scalar" $ do
      (v3 `mult` 5) `shouldBe` v1
      
    it "should multiply by negative scalar" $ do
      (v3 `mult` (-1)) `shouldBe` v5
      
  -- Dot Product Tests
  describe "Vector3 (Dot Product)" $ do
    it "should return same value regardless of Vector3 input order" $ do
      ((v9 `dot` v10)==(v10 `dot` v9)) `shouldBe` True
    
    it "should return zero when dot product'd with zero vector" $ do
      (v1 `dot` v0) `shouldBe` 0
      
  -- Cross Product Tests
  describe "Vector3 (Cross Product)" $ do
    it "should return the negative of Vector3 (a cross b) for Vector3 (b cross a)" $ do
      (v8 `cross` v9) `shouldBe` neg (v9 `cross` v8)
      
    it "should return the zero vector if a vector is crossed with the zero vector" $ do
      (v8 `cross` v0) `shouldBe` v0

  -- Magnitude Tests
  describe "Vector3 (Magnitude)" $ do
    it "should return zero for zero vector" $ do
      mag v0 `shouldBe` 0
      
    it "should return 1 for unit vector" $ do
      mag xUnit `shouldBe` 1
      
    it "should return 1 for negative unit vector" $ do
      mag (neg xUnit) `shouldBe` 1
    
  -- Normalization Tests
  describe "Vector3 (Normalize)" $ do
    it "should have magnitude of 1 (for non zero vector)" $ do
      mag (normalize v9) `shouldBe` 1
  
    it "should return zero vector given zero vector" $ do
      normalize v0 `shouldBe` v0

    it "should return xUnit vector given xUnit vector" $ do
      normalize xUnit `shouldBe` xUnit
      
    it "should not return ones vector given ones vector" $ do
      (normalize v1)==v1 `shouldBe` False


  -- Angle Between Tests
  describe "Vector3 (Angle Between)" $ do
    it "should return zero given two equal vectors" $ do
      angle_between v6 v6 `shouldBe` 0
  
    it "should return pi given a vector and its negative" $ do
      angle_between v6 (neg v6) `shouldBe` pi
  
  
  -- Scalar Projection Tests
  describe "Vector3 (Scalar Projection)" $ do
    it "should return magnitude of input vector if said vector is projected on itself" $ do
      proj v6 v6 `shouldBe` mag v6
  
    it "should return zero if a vector is projected along the zero vector" $ do
      (proj v6 v0) < 0.000001 `shouldBe` True
      
    it "should return zero if the zero vector is projected along another vector" $ do
      (proj v0 v9) < 0.000001 `shouldBe` True
      
      

