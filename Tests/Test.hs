--Tests for the pdb parser
--Try your own PDB as an extra precaution
module Main where

import PDButil.PDBparse
import PDBtools.Base

main = do
  contents <- parse "3C22.pdb"
  let bbone = backbone $ head $ fst contents
  print bbone


