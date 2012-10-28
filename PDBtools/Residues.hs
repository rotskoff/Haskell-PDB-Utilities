-- Module	: Residues
-- Copyright	: (c) 2012 Grant Rotskoff
-- License 	: GPL-3
--
-- Maintainer 	: gmr1887@gmail.com
-- Stability 	: experimental

-- A module providing residue selection groups. 

module Residues where

import Base
import PDButil.PDBparse

--One should really only use these methods on proteins, but for the sake of composing selections, the input form is [Atom]

charged :: [Atom] -> [Atom]
charged atms = restype "ASP" atms 
            ++ restype "GLU" atms 
            ++ restype "ARG" atms 
            ++ restype "LYS" atms 
            ++ restype "HIS" atms

uncharged :: [Atom] -> [Atom]
uncharged atms = filter (\s -> elem s $ charged atms) atms

polar :: [Atom] -> [Atom]
polar atms = charged atms
		     ++ restype "SER" atms 
             ++ restype "THR" atms
             ++ restype "ASN" atms 
             ++ restype "GLN" atms 

nonpolar :: [Atom] -> [Atom]
nonpolar atms = filter (\s -> elem s $ polar atms) atms

hydrophobic :: [Atom] -> [Atom]
hydrophobic atms = restype "ILE" atms
                ++ restype "LEU" atms
                ++ restype "VAL" atms
                ++ restype "PHE" atms
                ++ restype "CYS" atms
                ++ restype "MET" atms
                ++ restype "ALA" atms

hydrophillic :: [Atom] -> [Atom]
hydrophillic = undefined

-- Given a resname, provide the structure in the standard datatype. 
-- Coordinates are centered around the carbon alpha
-- The C-alpha N bond lies along the x-axis, with N in the negative direction
structure :: String -> [Atom]
structure = undefined
