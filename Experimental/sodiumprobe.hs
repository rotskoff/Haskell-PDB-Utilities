-- How does the binding environment affect desolvation?
module Main where

import Data.List
import System (getArgs)
import System.Directory
import System.IO (FilePath)
import PDBtools.PDBparse
import PDBtools.PDButil
import PDBtools.Vectors


main :: IO ()
main = do
  pdbDir <- getArgs
  setCurrentDirectory $ head pdbDir
  d <- getCurrentDirectory
  pdbList <- getDirectoryContents d
  let pdbListClean = filter (\s -> elem 'b' (show s)) pdbList
  putStrLn "\nExperimental Coordinates\n______________\n"
  putStr "PDB File\tAverage Na-O Dist.\tMin Na-O Dist.\tMax Na-O Dist.Max\t# Waters within 5.0A\t# Atoms within 5.0A\n"
  mapM_ generateReport pdbListClean

 
--Get the waters around the sodium ion
centerAtSodium :: [Atom] -> [Atom]
centerAtSodium cofactor = center sodium $ withinClusive 5.0 sodium cofactor  where
  sodium = head $ atomtype "NA" cofactor

--Count the number of waters within 5.0 angstrom of the sodium
environmentSize :: [Atom] -> Int
environmentSize = length . centerAtSodium

--Order them for comparison to the ideal models
groupBySize :: Int -> [[Atom]] -> [[Atom]]
groupBySize numatms atms = filter (\s -> environmentSize s == numatms) atms


generateReport :: FilePath -> IO()
generateReport pdbFile = do
  cofactor <- parseCofactorOnly pdbFile
  protein <- parseProteinOnly pdbFile
  let totalAtoms = (concatMap atoms protein) ++ cofactor 
  let waters = restype "HOH" $ centerAtSodium cofactor  
  if (length waters == 0) then do
      putStr $ (show pdbFile)++"\t0\t0\t0\t0\t0\n"
    else do
      let distanceList = map (norm . coords) waters
      let min = head $ sort distanceList
      let max = last $ sort distanceList
      let avg = (sum $ sort distanceList) / fromIntegral(length(distanceList))
      putStr $ (show pdbFile)++"\t"++(show avg)++"\t"++(show min)++"\t"++(show max)++"\t"++
               (show $ environmentSize $ cofactor)++"\t"++(show $ environmentSize $ totalAtoms)++"\n"



