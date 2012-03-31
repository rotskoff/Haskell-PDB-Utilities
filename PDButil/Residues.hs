-- Residues centered at the Carbon-Alpha, some sort of directionality constraint

module PDButil.Residues where

import PDButil.PDButil
import PDButil.PDBparse

--One should really only use these methods on proteins, but for the sake of composing selections, the input form is [Atom]

{-
charged :: [Atom] -> [Atom]
charged atms =           restype "ASP" atms 
                      ++ restype "GLU" atms 
                      ++ restype "ARG" atms 
                      ++ restype "LYS" atms 
                      ++ restype "HIS" atms

uncharged :: [Atom] -> [Atom]
uncharged atms = filter (elem $ charged atms) atms

polar :: [Atom] -> [Atom]
polar atms = charged ++ restype "SER" atms 
                     ++ restype "THR" atms
                     ++ restype "ASN" atms 
                     ++ restype "GLN" atms 

nonpolar :: [Atom] -> [Atom]
nonpolar atms = filter (elem $ polar atms) atms

hydrophobic :: [Atom] -> [Atom]
hydrophobic = nonpolar

hydrophillic :: [Atom] -> [Atom]
hydrophillic = polar 

-}
