module Utils.Constants where

import Control.Lens
import Data.List (sort)
import Types.Atom

-- | Get VDW radius.
vdwr ::
  Atom ->
  Double
vdwr a =
  case a ^. aelement of
    "H" -> 1.200
    "O" -> 1.520
    "N" -> 1.550
    "C" -> 1.700
    "S" -> 1.900
    "Br" -> 1.900
    "Cl" -> 1.750
    "F" -> 1.470
    othrewise -> error ("vdwr not found for: " ++ show a)

-- | Get hard sphere radius.
radius ::
  -- | Element
  String ->
  -- | sphere radius
  Double
radius a =
  case a of
    "H" -> 0.330
    "O" -> 0.660
    "C" -> 0.770
    othrewise -> error ("radius not found for: " ++ show a)

-- | Get bound distance.
bndist ::
  -- | First element
  String ->
  -- | Second element
  String ->
  -- | Bond type
  Int ->
  -- | Bond distance
  Double
bndist a b t =
  let s = concat (sort [a, b])
   in case (s, t) of
        ("CC", 1) -> 1.54
        ("CC", 2) -> 1.34
        ("CC", 3) -> 1.20
        ("CH", 1) -> 1.09
        ("CO", 1) -> 1.43
        ("HO", 1) -> 0.96
        ("HN", 1) -> 1.45
        ("CN", 1) -> 1.35
        ("BrC", 1) -> 1.94
        ("CCl", 1) -> 1.76
        ("CF", 1) -> 1.36
        ("CO", 2) -> 1.24
        ("NN", 2) -> 1.24
        otherwise ->
          error
            ( "bndist not found for: " ++ s
                ++ " type "
                ++ show t
            )
