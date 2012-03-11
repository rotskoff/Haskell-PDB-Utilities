module Vectors where

-- A minimal implementation of vector operations for pdb calculations. 
-- These vector operations are NOT safe... they will not verify dimensionality requirements
-- TODO; matrix multiplication, factorizations.

import Data.List

dot :: (Num a) => [a] -> [a] -> a
dot a b = foldr1 (+) $ zipWith (*) a b

-- Only defined on 3 dimensional vectors; no obvious generalization
cross :: (Num a) => [a] -> [a] -> [a]
cross [a1,a2,a3] [b1,b2,b3] = [c1,c2,c3] where
	c1 = a2*b3 - a3*b2
	c2 = a3*b1 - a1*b3
	c3 = a1*b2 - a2*b1

vAdd :: (Num a) => [a] -> [a] -> [a]
vAdd = zipWith (+)

vSub :: (Num a) => [a] -> [a] -> [a]
vSub = zipWith (-)

magnitude :: (Num a) => [a] -> a
magnitude = sum . (map (^2))

norm :: (Floating a) => [a] -> a
norm = sqrt . magnitude

unit :: (Floating a) => [a] -> [a]
unit vec = map (/ (norm vec)) vec

angle :: (Floating a) => [a] -> [a] -> a
angle a b
	| norm a == 0 || norm b == 0 = 0
  | otherwise = acos $ (a `dot` b) / ((norm a) * (norm b))

-- Two Dimensions
vRotate :: (Floating a) => a -> [a] -> [a]
vRotate degs [x,y] = mtimes rMatrix [x,y] where
  rMatrix = [[cos(degs),-sin(degs)],[sin(degs),cos(degs)]]

vRotate3d :: (Floating a) => a -> a -> a -> [a] -> [a]
vRotate3d theta phi psi vect = [r1 `dot` vect, r2 `dot` vect, r3 `dot` vect] where
  rMatrix = [r1,r2,r3]
  r1 = [cos(theta)*cos(psi),-cos(phi)*sin(psi)+sin(phi)*sin(theta)*cos(psi),sin(phi)*sin(psi)+cos(phi)*sin(theta)*cos(psi)]
  r2 = [cos(theta)*sin(psi),cos(phi)*cos(psi)+sin(phi)*sin(theta)*sin(psi),-sin(phi)*cos(psi)+cos(phi)*sin(theta)*sin(psi)]
  r3 = [-sin(theta),sin(phi)*cos(theta),cos(phi)*cos(theta)]
