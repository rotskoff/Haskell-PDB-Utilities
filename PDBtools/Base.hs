-- Module	: Base, the standard functions
-- Copyright	: (c) 2012 Grant Rotskoff
-- License 	: GPL-3
--
-- Maintainer 	: gmr1887@gmail.com
-- Stability 	: experimental


-- The suite of source files in the PDButil directory are meant to complement high-throughput analyses
-- of three-dimensional protein structure data in the PDB format. Because the source files rely heavily
-- on one another, it is convenient to import them all into a single module. Examples of analysis projects
-- are available at http://www.github.com/rotskoff/Haskell-PDB-Utilities 

module Base where

-- Long list of imports...
import PDButil.PDBparse
import PDButil.Vectors
import Data.List
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import Data.Maybe

-- Pull all atoms of a given name from a list of atoms
atomtype :: String -> [Atom] -> [Atom]
atomtype t atmlist = filter matcht atmlist where
   matcht a = atype a == B.pack t
   
-- Match the atom's name in the PDB file rather than the underlying type   
atomname :: String -> [Atom] -> [Atom]
atomname t atmlist = filter matcht atmlist where
   matcht a = name a == B.pack t

atomnames :: [String] -> [Atom] -> [Atom]
atomnames ts atmlist = filter matchts atmlist where
    matchts a = name a `elem` (map B.pack ts)

-- Match the residue type, input the three letter abbreviation
restype :: String -> [Atom] -> [Atom]
restype t atmlist = filter matcht atmlist where
   matcht a = resname a == B.pack t

-- Extract the list of alpha-Carbons from a protein
backbone :: Protein -> [Atom]
backbone = atomname "CA" . atoms

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
-- Only well-defined if you input the same molecule!
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

dihedrals :: Protein -> [(Double,Double)]
dihedrals p = map (\cAlpha -> dihedral cAlpha (atoms p)) $ backbone p

kth :: Int -> Atom -> String -> [Atom] -> [Double]
kth k cAlpha t atms
    | (getAdjacent k cAlpha t atms) == [] = [0,0,0]
    | otherwise = coords $ head $ getAdjacent k cAlpha t atms

getAdjacent :: Int -> Atom -> String -> [Atom] -> [Atom]
getAdjacent k cAlpha t atms = filter (\s -> resid s == resid cAlpha+k) $ atomname t atms 


--TODO TESTING
dihedral :: Atom -> [Atom] -> (Double,Double)
dihedral cAlpha atms
    | prevCO == [0,0,0] || nextN == [0,0,0] = (0,0)
    | name cAlpha == B.pack "CA" = (phi v1 v2,psi v2 v3)
    | otherwise = error "Please input a Carbon Alpha atom." where
    prevCO = kth (-1) cAlpha "C" atms
    currCA = coords $ cAlpha
    currN = kth 0 cAlpha "N" atms
    currCO = kth 0 cAlpha "C" atms
    nextN = kth 1 cAlpha "N" atms
    a = vSub prevCO currCA
    b = vSub currN currCA
    c = vSub currCA currCO
    d = vSub currCA nextN
    v1 = a `cross` b
    v2 = c `cross` b
    v3 = c `cross` d
    phi v1 v2
        | a `dot` v1 < 0 =  angle v1 v2
        | otherwise = -(angle v1 v2)
    psi v2 v3
        | d `dot` v3 > 0 = angle v2 v3
        | otherwise = -(angle v2 v3)

rama :: Protein -> IO()
rama p = do
  let toTSV (phi,psi) = (show phi) ++ "\t" ++ (show psi) 
  mapM_ putStrLn $ map toTSV $ dihedrals p


-- Convert a protein to FASTA sequence format
-- TODO, headers in FASTA file spec
protein2fasta :: Protein -> ByteString
protein2fasta protein = B.pack $ concatMap (\s -> convert (resname s)) (backbone protein)

convert :: ByteString -> String  
convert name
   | query == Nothing = "X" 
   | otherwise = fromJust query where 
   query = Map.lookup (B.unpack name) resMap
   resMap = Map.fromList 
    [("ALA","A"),
     ("CYS","C"),
     ("ASP","D"),
     ("GLU","E"),
     ("PHE","F"),
     ("GLY","G"),
     ("HIS","H"),
     ("ILE","I"),
     ("LYS","K"),
     ("LEU","L"),
     ("MET","M"),
     ("ASN","N"),
     ("PYL","O"),
     ("PRO","P"),
     ("GLN","Q"),
     ("ARG","R"),
     ("SER","S"),
     ("THR","T"),
     --Selenocysteine
     ("VAL","V"),
     ("TRP","W"),
     ("TYR","Y")]
     -- otherwise, use 'X'
