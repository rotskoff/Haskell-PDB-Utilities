-- How does the binding environment affect desolvation?
module Main where

import Data.List
import System (getArgs)
import System.Directory
import System.IO (FilePath)
import PDBparse
import PDButil
import IdealDesolvationCalcium
import Vectors


main :: IO ()
main = do
  pdbDir <- getArgs
  setCurrentDirectory $ head pdbDir
  d <- getCurrentDirectory
  pdbList <- getDirectoryContents d
  let pdbListClean = filter (\s -> elem 'b' (show s)) pdbList
  putStrLn "\nExperimental Coordinates\n______________\n"
  putStr "PDB File\tAverage Ca-O Dist.\tMin Ca-O Dist.\tMax Ca-O Dist.Max\t# Waters within 5.0A\t# Atoms within 5.0A\n"
  mapM_ generateReport pdbListClean

 
--Get the waters around the calcium ion
centerAtCalcium :: [Atom] -> [Atom]
centerAtCalcium cofactor = center calcium $ withinClusive 5.0 calcium cofactor  where
  calcium = head $ atomtype "CA" cofactor

--Count the number of waters within 5.0 angstrom of the calcium 
environmentSize :: [Atom] -> Int
environmentSize = length . centerAtCalcium

--Put th
groupBySize :: Int -> [[Atom]] -> [[Atom]]
groupBySize numatms atms = filter (\s -> environmentSize s == numatms) atms


generateReport :: FilePath -> IO()
generateReport pdbFile = do
  cofactor <- parseCofactorOnly pdbFile
  protein <- parseProteinOnly pdbFile
  let totalAtoms = (concatMap atoms protein) ++ cofactor 
  let waters = restype "HOH" $ centerAtCalcium cofactor  
  if (length waters == 0) then do
      putStr $ (show pdbFile)++"\t"++"Average: 0\tMin: 0\tMax: 0\n"
    else do
      let distanceList = map (norm . coords) waters
      let min = head $ sort distanceList
      let max = last $ sort distanceList
      let avg = (sum $ sort distanceList) / fromIntegral(length(distanceList))
      putStr $ (show pdbFile)++"\t"++(show avg)++"\t"++(show min)++"\t"++(show max)++"\t"++
               (show $ environmentSize $ cofactor)++"\t"++(show $ environmentSize $ totalAtoms)++"\n"





{-
alignWithIdeal :: Atom -> [Atom] -> Atom -> [Atom]
alignWithIdeal a atms idealAtm = rotate atms caPivot a (coords idealAtm) where
  caPivot = head $ atomtype "CA" atms
-}

-- Give RMSD for desolvation patterns of Ca from the ideal versions...
-- Append to file with PDB name...
{-
deviationReport :: FilePath -> IO()
deviationReport pdb = undefined
  cofactor <- parseCofactorOnly pdb
-}



