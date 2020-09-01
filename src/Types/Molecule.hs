{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Molecule where

import Control.Lens
import Data.List (find)
import qualified Data.IntMap as M
import Types.Atom

data Bond = Bond
  { _bfid   :: Int
  , _bsid   :: Int
  , _btype  :: Int
  , _bster  :: Int
  , _btop   :: Int
  }
  deriving (Show)

bond = Bond 
  { _bfid = 0
  , _bsid = 0 
  , _btype = 0
  , _bster = 0 
  , _btop = 0
  }

makeLenses ''Bond

data Molecule = Molecule 
  { _atoms :: M.IntMap Atom
  , _bonds :: [Bond]
  }
  deriving (Show)

molecule = Molecule 
  { _atoms = M.empty
  , _bonds = []
  }

makeLenses ''Molecule

data ZMolecule = ZMolecule
  { _zatoms :: [ZAtom]
  , _zresseq :: Int
  , _zresname :: String
  }

zmolecule = ZMolecule
  { _zatoms = []
  , _zresseq = 0
  , _zresname = ""
  }

makeLenses ''ZMolecule