{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : InsertMolecule
-- Copyright   : (c) Vusal Salmanov, 2019
-- Maintainer  : salmanov.vh@gmail.com
-- Stability   : experimental
--
-- The module describes the work with functions that perform the insertion.
module Programs.InsertMolecule where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import qualified Data.IntMap as M
import qualified Data.List as L
import Data.Maybe
import Linear hiding (ei, ej, ek)
import Numeric (acos, sqrt)
import System.Process (callCommand)
import Types.Atom
import Types.Molecule
import Utils.Operators
import Utils.ReadWrite
import Utils.TrigDegree

-- ============> Set Atoms with/without Optimization <================
setAtomWithOutOptimization :: Int -> ZMolecule -> Molecule -> Molecule -> [Molecule]
setAtomWithOutOptimization n zMol oMol = undefined -- do
-- let zAtom = (zMol ^. zatoms) !! n
--     newAtom = uploadData zAtom atom
--     [idA, idB, idC, idD] = map (zatom ^.) [serial, con, valcon, dihcon]
--     [dist', valcon', dihcon'] = map (zatom ^.) [dist, valangl, dihangl]
--     atomB = getAtomById idB oMol
--     atomC = getAtomById idC oMol
--     atomD = getAtomById idD oMol
-- let possibleAtoms a b = rotateAtom ej b . rotateAtom ek a
--     allVariance = possibleAtoms <$> map deg2Rad alpha <*> map deg2Rad beta
-- goodVariance <- filter (\x -> not (x `isIntersection` oMol)) (allVariance atom)
-- return undefined -- $ M.insert atomID_B goodVariance insMol

-- ============> Set Bonds 1-2, 1-3, 1-4 <================
setBond12 :: (Atom, Atom, Angstroms) -> Int -> Molecule -> [Molecule]
setBond12 (a, b, Angstroms x) i mol = do
  let b . acoords = (a ^. acoords) & _x +~ x
      possibleCoord a b x = rotateAtom ej b (rotateAtom ek a x)
  map (\x -> addAtom (i, x) mol) (liftA3 possibleCoord alpha beta (pure b))

setBond13 :: (Atom, Atom, Atom, Double, Double) -> Molecule -> [Molecule]
setBond13 (a, b, c, x, y) mol = error "to do"

setBond14 :: (Atom, Atom, Atom, Atom, Double, Double, Double) -> Molecule -> [Molecule]
setBond14 (a, b, c, d, x, y, z) mol = error "to do"

-- ============> Auxuliar Angles <================
alpha = [Degree 0, Degree 1 .. Degree 359]

beta = [Degree (-90), Degree (-89) .. Degree 90]

-- ============> Auxuliar Base Vectors <================
ei = V3 1 0 0

ej = V3 0 1 0

ek = V3 0 0 1

addAtom :: (Int, Atom) -> Molecule -> Molecule
addAtom (i, x) = over atoms (M.insert i x)
{-# INLINE addAtom #-}

getAtomById :: Int -> Molecule -> Maybe Atom
getAtomById id mol = M.lookup id (mol ^. atoms)
{-# INLINE getAtomById #-}

isIntersection :: Atom -> Molecule -> Bool
isIntersection i mol =
  let ids = map snd (M.toList $ mol ^. atoms)
      isIntersection' a b = distance v1 v2 <= (r1 + r2)
        where
          (v1, r1) = (a ^. acoords, a ^. vdwr)
          (v2, r2) = (b ^. acoords, b ^. vdwr)
   in any (isIntersection' i) ids

rotateAtom :: Point -> Degree Double -> Atom -> Atom
rotateAtom axis angle atom =
  let newCoords = rotate (axisAngle axis (deg2Rad angle)) (atom ^. acoords)
   in atom & acoords .~ newCoords

valAngle :: Atom -> Atom -> Double
valAngle a b =
  let dist01 = norm (a ^. acoords)
      dist02 = norm (b ^. acoords)
      dist12 = distance (a ^. acoords) (b ^. acoords)
      angl = (dist01 ** 2 + dist02 ** 2 - dist12 ** 2) / (2 * dist01 * dist02)
   in acos angl
{-# INLINE valAngle #-}
