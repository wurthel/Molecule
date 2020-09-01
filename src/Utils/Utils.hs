module Utils.Utils where

import Control.Lens
import Control.Monad.State (execState)
import qualified Data.IntMap as M
import Data.List
import Linear.V3
import Types.Atom
import Types.Molecule

-- | Shift coordinate system
shiftCoordSystem :: Point -> Molecule -> Molecule
shiftCoordSystem zero = execState $ do
  let x0 = zero ^. _x
      y0 = zero ^. _y
      z0 = zero ^. _z
  atoms . traverse . acoords . _x -= x0
  atoms . traverse . acoords . _y -= y0
  atoms . traverse . acoords . _z -= z0

-- | Check bond
-- TODO: Тест на проверку функции. Bonds должен быть отсортирован таким образом, чтобы минимальные элементы
-- находились на первой позиции
isBondedById :: (Int, Int) -> Molecule -> Bool
isBondedById (a, b) m =
  let (a', b') = (min a b, max a b)
      bs = map (\x -> (x ^. bfid, x ^. bsid)) (m ^. bonds)
   in (a', b') `elem` bs

-- | Get bound
-- TODO: Тест на проверку функции. Bonds должен быть отсортирован таким образом, чтобы минимальные элементы
-- находились на первой позиции
getBondById ::
  (Int, Int) -> -- First and second atoms id (bfid and bsid)
  Molecule ->
  Maybe Bond
getBondById (a, b) m =
  let (a', b') = (min a b, max a b)
      bs = map (\x -> (x ^. bfid, x ^. bsid, x)) (m ^. bonds)
   in find (\x -> a' == x ^. bfid && b' == x ^. bsid) (m ^. bonds)

-- | Get atom by id
getAtom :: Int -> Molecule -> Maybe Atom
getAtom i m = M.lookup i (m ^. atoms)
