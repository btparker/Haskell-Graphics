module Vector3 where
-------------------------------------------------------------
-- 3D Vector types and functionality (right hand rule)


data Vector3 = Vector3 !Double !Double !Double deriving (Eq, Show)

add :: Vector3 -> Vector3 -> Vector3
(Vector3 a1 a2 a3) `add` (Vector3 b1 b2 b3) = Vector3 (a1+b1) (a2+b2) (a3+b3)

sub :: Vector3 -> Vector3 -> Vector3
(Vector3 a1 a2 a3) `sub` (Vector3 b1 b2 b3) = Vector3 (a1-b1) (a2-b2) (a3-b3)

neg :: Vector3 -> Vector3
neg (Vector3 x y z) = (Vector3 (-x) (-y) (-z))

mult :: Vector3 -> Double -> Vector3
mult (Vector3 x y z) c = (Vector3 (x*c) (y*c) (z*c))

dot :: Vector3 -> Vector3 -> Double
dot (Vector3 a1 a2 a3) (Vector3 b1 b2 b3) = a1*b1+a2*b2+a3*b3

cross :: Vector3 -> Vector3 -> Vector3
cross (Vector3 a1 a2 a3) (Vector3 b1 b2 b3) = (Vector3 (a2*b3-a3*b2) (a3*b1-a1*b3) (a1*b2-a2*b1))

mag :: Vector3 -> Double
mag = sqrt . sq_mag

sq_mag :: Vector3 -> Double
sq_mag v = v `dot` v

normalize :: Vector3 -> Vector3
normalize v
  | (mag v) /= 0 = mult v (1 / mag v) 
  | otherwise    = (Vector3 0 0 0)
                   
angle_between :: Vector3 -> Vector3 -> Double
angle_between a b 
  | result < -1 = acos (-1) -- clamping to avoid floating point issues
  | result > 1 = acos 1
  | otherwise = acos result
                where result = (normalize a) `dot` (normalize b)
                      
proj :: Vector3 -> Vector3 -> Double
proj a b = (mag a) * (cos (angle_between a b))