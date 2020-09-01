{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Atom where

import Control.Lens
import Linear.V3

type Serial = Int

type Resseq = Int

type Resname = String

type Element = String

type Point = V3 Double

newtype Angstroms = Angstroms Double deriving (Show, Eq)

data Atom = Atom
  { -- | "ATOM"/"HETATM"
    _adatatype :: String,
    -- | Atom serial number
    _anumeric :: Int,
    -- | Atom name
    _aname :: String,
    -- | Alternate location indicator.
    _aaltloc :: String,
    -- | Residue name
    _aresname :: String,
    -- | Chain identifier
    _achainid :: String,
    -- | Residue sequence number
    _aresseq :: Int,
    -- | Code for insertions of residues
    _arescode :: String,
    -- | (X,Y,Z) orthogonal Å coordinate
    _acoords :: Point,
    -- | Occupancy
    _aoccup :: Double,
    -- | Temperature factor
    _atempfac :: Double,
    -- | Element symbol
    _aelement :: String,
    -- | Atom charge
    _acharge :: String,
    _vdwr :: Double
  }
  deriving (Show, Eq)

atom =
  Atom
    { _adatatype = "ATOM",
      _anumeric = 0,
      _aname = "",
      _aaltloc = "",
      _aresname = "",
      _achainid = "",
      _aresseq = 0,
      _arescode = "",
      _acoords = V3 0 0 0,
      _aoccup = 0,
      _atempfac = 0,
      _aelement = "",
      _acharge = "",
      _vdwr = 0
    }

makeLenses ''Atom

data ZAtom = ZAtom
  { _serial :: Int,
    _con :: Int,
    _dist :: Double,
    _valcon :: Int,
    _valangl :: Double,
    _dihcon :: Int,
    _dihangl :: Double,
    _name :: String
  }

zatom =
  ZAtom
    { _serial = 0,
      _con = 0,
      _dist = 0,
      _valcon = 0,
      _valangl = 0,
      _dihcon = 0,
      _dihangl = 0,
      _name = ""
    }

makeLenses ''ZAtom
