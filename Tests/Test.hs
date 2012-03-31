--Tests for the pdb parser
--Try your own PDB as an extra precaution
module Main where

import PDBtools.PDButil
import PDBtools.PDBparse

main = do
  contents <- parse "3C22.pdb"
  let bbone = backbone $ head $ fst contents
  print bbone


