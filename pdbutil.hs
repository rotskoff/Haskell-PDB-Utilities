--PDB Utilities, functions for basic manipulation of PDB data

module PDButil where

import Data.List
import PDBparse
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Vectors

-- Pull all atoms of a given name from a list of atoms
atomtype :: String -> [Atom] -> [Atom]
atomtype t atmlist = filter matcht atmlist where
   matcht a = atype a == B.pack t

restype :: String -> [Atom] -> [Atom]
restype t atmlist = filter matcht atmlist where
   matcht a = resname a == B.pack t

-- Extract the list of alpha-Carbons from a protein
backbone :: Protein -> [Atom]
backbone = atomtype "CA" . atoms

-- Extract the list of residue name, residue number pairs
resSeq :: Protein -> [(ByteString,Int)]
resSeq p = zip (map resname bbatms) (map resid bbatms) where
    bbatms = backbone p

--Naive homology calculation
--this could be greatly improved by some alignment effort
homology :: Protein -> Protein -> Double
homology a b = (fromIntegral identities) / (fromIntegral totalLength) where
    identities = 2 * length (resSeq a `intersect` resSeq b)
    totalLength = length(resSeq a) + length(resSeq b)

-- Euclidean Distance between two atoms
distance :: Atom -> Atom -> Double
distance a1 a2 = norm $ vSub (coords a1) (coords a2)

-- Root Mean Squared Deviation, a measure of the total distance change
-- Only well-defined if you input the same protein!
rmsd :: [Atom] -> [Atom] -> Double
rmsd atms atms' = sqrt $ avg sqdist where
    avg ds = (1/fromIntegral(length(ds))) * sum(ds)
    sqdist = map (^2) $ zipWith distance (atms) (atms')

-- Collect all the atoms within a given distance 
within :: Double -> Atom -> [Atom] -> [Atom]
within range a = (delete a) . filter withinRange where
	withinRange a' = (distance a a') <= range

withinClusive :: Double -> Atom -> [Atom] -> [Atom]
withinClusive range a = filter withinRange where
	withinRange a' = (distance a a') <= range


-- Centers the list of atoms around the specified atom
center :: Atom -> [Atom] -> [Atom]
center a = a `shift` [0,0,0]

-- Shift the entire atomlist by specifying the new location of a single atom
shift :: Atom -> [Double] -> [Atom] -> [Atom]
shift a newCoords as = map (\s -> s {coords = (translate s)}) as where
  translate s = vAdd (coords s) shiftFactor
  shiftFactor = vSub newCoords (coords a) 

-- Global translate by a vector
translateBy :: [Double] -> [Atom] -> [Atom]
translateBy vect = map (\s -> s {coords = (vAdd (coords s) vect)})

-- Compute the angle between three atoms, return value in radians!
atmAngle :: Atom -> Atom -> Atom -> Double
atmAngle a b c = angle baVec bcVec where
	baVec = (coords a) `vSub` (coords b)
	bcVec = (coords c) `vSub` (coords c)

{-
-- TODO Fix so that it works!
rotateAboutOrigin :: [Atom] -> Atom -> [Double] -> [Atom]
rotateAboutOrigin atms tracer destination = map (\s -> s {coords = (translate (coords s))}) atms where
  [x,y,z] = destination
  [x',y',z'] = coords tracer
  psi = angle [y',z'] [y,z] --yz angle
  p = angle [x',z'] [x,z] --xz angle
  phi = angle [x',y'] [x,y] --xy angle
  translate = vRotate3d theta psi phi

-- Rotate a list of atoms about a fixed atom by moving the selected atom to a specified destination
rotate :: [Atom] -> Atom -> Atom -> [Double] -> [Atom]
rotate atms pivot tracer destination = translateBy (coords pivot) rotatedAtms where
  centeredAtPivot = center pivot atms
  rotatedAtms = rotateAboutOrigin centeredAtPivot tracer destination

-}





