module Vector3 where
-------------------------------------------------------------
-- 3D Vector types and functionality (right hand rule)


data Vector3 = Vector3 !Double !Double !Double deriving (Eq, Show)

add :: Vector3 -> Vector3 -> Vector3
(Vector3 x y z) `add` (Vector3 a b c) = Vector3 (a+x) (b+y) (c+z)

sub :: Vector3 -> Vector3 -> Vector3
(Vector3 x y z) `sub` (Vector3 a b c) = Vector3 (x-a) (y-b) (z-c)

neg :: Vector3 -> Vector3
neg (Vector3 x y z) = (Vector3 (-x) (-y) (-z))

mult :: Vector3 -> Double -> Vector3
mult (Vector3 x y z) c = (Vector3 (x*c) (y*c) (z*c))

dot :: Vector3 -> Vector3 -> Double
dot (Vector3 x y z) (Vector3 a b c) = x*a + b*y + c*z

cross :: Vector3 -> Vector3 -> Vector3
cross (Vector3 a b c) (Vector3 x y z) = (Vector3 (b*z + c*y) (-1*(a*z + c*x)) (a*y + b*x))

sq_mag :: Vector3 -> Double
sq_mag v = v `dot` v

mag :: Vector3 -> Double
mag = sqrt . sq_mag

