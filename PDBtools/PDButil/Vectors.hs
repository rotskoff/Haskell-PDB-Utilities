-- Module	: Vectors
-- Copyright	: (c) 2012 Grant Rotskoff
-- License 	: GPL-3
--
-- Maintainer 	: gmr1887@gmail.com
-- Stability 	: experimental

module PDButil.Vectors where
-- A minimal implementation of vector operations for pdb calculations. 
-- These vector operations are NOT safe... they will not verify dimensionality requirements
-- TODO; matrix multiplication, factorizations.

import Data.List

dot :: (Num a) => [a] -> [a] -> a
dot a b = foldr1 (+) $ zipWith (*) a b

-- Only defined on 3 dimensional vectors
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

norm :: (Num a, Floating a) => [a] -> a
norm = sqrt . magnitude

unit :: (Num a, Floating a) => [a] -> [a]
unit vec = map (/ (norm vec)) vec

angle :: (Eq a, Floating a) => [a] -> [a] -> a
angle a b
    | norm a == 0 || norm b == 0 = 0
    | otherwise = (180/pi) * (acos $ (a `dot` b) / ((norm a) * (norm b)))

rotateAboutz :: (Floating a) => [a] -> a -> [a]
rotateAboutz vec degs = map (dot vec) rMatrix where
    rMatrix = [[cos(theta),sin(theta),0],[-sin(theta),cos(theta),0],[0,0,1]]
    theta = degs*(pi/180)

rotateAboutx :: (Floating a) => [a] -> a -> [a]
rotateAboutx vec degs = map (dot vec) rMatrix where
    rMatrix = [[1,0,0],[0,cos(theta),sin(theta)],[0,-sin(theta),cos(theta)]]
    theta = degs*(pi/180)



