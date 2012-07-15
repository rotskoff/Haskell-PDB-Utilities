-- Module	: PDBparse
-- Copyright	: (c) 2012 Grant Rotskoff
-- License 	: GPL-3
--
-- Maintainer 	: gmr1887@gmail.com
-- Stability 	: experimental

module PDButil.PDBparse where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import System.IO (FilePath)

data Atom =    Atom    { name     :: ByteString,
                         atid     :: Int,
                         chain    :: ByteString,
                         resid    :: Int,
                         resname  :: ByteString,
                         coords   :: [Double],
                         aField   :: Double,
                         bField   :: Double,
                         atype    :: ByteString    }
               deriving (Show,Eq)

data Protein = Protein { atoms    :: [Atom] }
               deriving (Show)

--Sample record:
-- ATOM      1  N   ASP A  28      52.958  39.871  41.308  1.00 89.38           N  

{- We only want record lines that begin with ATOM and HETATM
   ATOM lines contain the coordinates of the protein(s) in a PDB file 
   HETATM lines (short for heteroatom) contain coordinate information for 
   other molecules present in the structure... ligands, DNA, RNA, waters, etc. -}

parseAtom :: ByteString -> Atom
parseAtom record = Atom {   name = pull 13 16, 
                            atid = rpull 7 11,
                           chain = pull 22 22,
                           resid = rpull 23 26,
                         resname = pull 18 20,   
                          coords = [rpull 31 38,rpull 39 46,rpull 47 54],
                          aField = rpull 55 60, 
                          bField = rpull 61 66,
                           atype = pull 77 78  } where

  --Hard coded parsing of the PDB record for coordinate types
  --I've encountered this "repacking for comparison in expert code, 
  --but it seems like comparison should be possible some other way

   pull m n = cutspace $ B.drop (m-1) $ B.take n record
   rpull m n = read $ B.unpack $ pull m n  
   cutspace = B.pack . filter (/=' ') . B.unpack 


isAtom :: ByteString -> Bool
isAtom line = (B.take 4 line) == (B.pack "ATOM")

isHETATM :: ByteString -> Bool
isHETATM line = (B.take 6 line) == (B.pack "HETATM")


parse :: FilePath -> IO ([Protein],[Atom])
parse pdb = do
    let input = B.readFile pdb
    bstring <- input
    let atms = map parseAtom $ filter isAtom (B.lines bstring)
    let hetatms = map parseAtom $ filter isHETATM (B.lines bstring)
    return (splitChains atms, hetatms)

parseCofactorOnly :: FilePath -> IO [Atom]
parseCofactorOnly pdb = do 
	bstring <- B.readFile pdb
	let hetatms = map parseAtom $ filter isHETATM (B.lines bstring)
	return hetatms

parseProteinOnly :: FilePath -> IO [Protein]
parseProteinOnly pdb = do
	bstring <- B.readFile pdb
	let atms = map parseAtom $ filter isAtom (B.lines bstring)
	return $ splitChains atms

splitChains :: [Atom] -> [Protein]
splitChains [] = []
splitChains contents = [Protein {atoms = chain1}] ++ splitChains remainder where
	chain1 = takeWhile (\s -> id == chain s) contents
	remainder = dropWhile (\s -> id == chain s) contents
	id = chain (head contents)

--TODO 
-- HELIX lines in PDB file. 
getSecondaryStructure :: Protein -> [([Atom],String)]
getSecondaryStructure = undefined
 
