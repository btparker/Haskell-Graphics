module Tests where
import TestFramework
import Vector3

v0 = Vector3 0 0 0
v1 = Vector3 5 5 5
v2 = Vector3 5.0 5.0 5.0
v3 = Vector3 1 1 1
v4 = Vector3 2 2 2
v5 = Vector3 (-1) (-1) (-1)
v6 = Vector3 3 (-4) (-1)
v7 = Vector3 0 5 2

runtests = do
  -- ** Vector3 Tests ** --
  
  -- Equality Tests
  putStrLn "1: Equality Tests"
  assertTrue (v0==v0) "-- 1.1 - Vector3 equal to self"
  assertTrue (v1==v2) "-- 1.2 - Vector3 equal to int init and float init"  
  assertFalse (v0==v1) "-- 1.3 - Vector3 not equal to two unequal Vector3"  

  -- Addition Tests
  putStrLn "2: Addition Tests"
  assertTrue ((v3 `add` v3)==v4) "-- 2.1 - Vector3 add same"
  assertTrue ((v3 `add` v5)==v0) "-- 2.2 - Vector3 add negative"
  
  -- Subtraction Tests
  putStrLn "3: Subtraction Tests"
  assertTrue ((v3 `sub` v3)==v0) "-- 3.1 - Vector3 sub same"
  assertTrue ((v3 `sub` v5)==v4) "-- 3.2 - Vector3 sub negative"
  
  -- Negation Tests
  putStrLn "4: Negation Tests"
  assertTrue ((neg v3)==v5) "-- 4.1 - Negate positive"
  assertTrue ((neg v5)==v3) "-- 4.2 - Negate negative"
  
  -- Multiplication Tests
  putStrLn "5: Multiplication Tests"
  assertTrue ((v3 `mult` 5)==v1) "-- 5.1 - Vector multiply by positive scalar"
  assertTrue ((v3 `mult` (-1))==v5) "-- 5.2 - Vector multiply by negative scalar"
  
  -- Dot product Tests
  putStrLn "6: Dot Product Tests"
  
  -- Cross product Tests

  -- Magnitude Tests  

  -- Squared Magnitude Tests

  -- Normalization Tests