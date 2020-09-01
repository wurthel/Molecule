module Utils.ReadWrite
  ( -- * Read
    readMolV2000,
    readPdb,

    -- * Write
    writeXyz,
    writePdb,
    -- , writeBonds
  )
where

import Control.Lens
import Control.Monad.State (execState)
import qualified Data.IntMap as M
import Data.List (elem)
import Linear.V3
import System.Directory (renameFile)
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (hPrintf)
import Types.Atom
import Types.Molecule

-- | Read molecule in *.mol format (V2000)
readMolV2000 :: FilePath -> Molecule
readMolV2000 inf =
  let txt = (lines . unsafePerformIO . readFile) inf
      cnt_lines = words (txt !! 3)
      cnt_atoms = read (cnt_lines !! 0)
      cnt_bonds = read (cnt_lines !! 1)
      lsf = (take cnt_atoms . drop 4) txt
      lsg = (take cnt_bonds . drop (4 + cnt_atoms)) txt
      as = M.fromList $ foldr ((:) . f) [] (zip [1 ..] lsf)
      bs = foldr ((:) . g) [] lsg
   in execState
        ( do
            atoms .= as
            bonds .= bs
        )
        molecule
  where
    f (i, l) =
      ( i,
        execState
          ( do
              let w = words l
              acoords . _x .= read (w !! 0)
              acoords . _y .= read (w !! 1)
              acoords . _z .= read (w !! 2)
              aname .= w !! 3
              aelement .= w !! 3
          )
          atom
      )
    g l =
      execState
        ( do
            let w = words l
            bfid .= read (w !! 0)
            bsid .= read (w !! 1)
            btype .= read (w !! 2)
            bster .= read (w !! 3)
            btop .= read (w !! 4)
        )
        bond

-- | Read molecule in *.pdb format (V2000).
readPdb :: FilePath -> Molecule
readPdb inf =
  execState
    ( do
        let ls = (lines . unsafePerformIO . readFile) inf
            ls' = filter (\l -> rfld strip 1 6 l `elem` ["ATOM", "HETATM"]) ls
        atoms .= M.fromList (foldr ((:) . f) [] ls')
    )
    molecule
  where
    strip = filter (/= ' ')
    rfld f s e = f . drop (s - 1) . take e
    f l =
      ( rfld read 7 11 l,
        execState
          ( do
              adatatype .= rfld strip 1 6 l
              aname .= rfld strip 13 16 l
              aaltloc .= rfld strip 17 17 l
              aresname .= rfld strip 18 20 l
              achainid .= rfld strip 22 22 l
              aresseq .= rfld read 23 26 l
              arescode .= rfld strip 27 27 l
              acoords . _x .= rfld read 31 38 l
              acoords . _y .= rfld read 39 46 l
              acoords . _z .= rfld read 47 54 l
              aoccup .= rfld read 55 60 l
              atempfac .= rfld read 61 66 l
              aelement .= rfld strip 77 78 l
              acharge .= rfld strip 79 80 l
          )
          atom
      )

readZMol :: FilePath -> ZMolecule
readZMol inf =
  execState
    ( do
        let ls = (lines . unsafePerformIO . readFile) inf
        zresseq .= (read . (!! 2) . words . (!! 0)) ls
        zresname .= ((!! 2) . words . (!! 1)) ls
        zatoms .= foldr ((:) . f) [] (drop 2 ls)
    )
    zmolecule
  where
    f l =
      execState
        ( do
            let w = words l
            serial .= read (w !! 0)
            con .= read (w !! 1)
            dist .= read (w !! 2)
            valcon .= read (w !! 3)
            valangl .= read (w !! 4)
            dihcon .= read (w !! 5)
            dihangl .= read (w !! 6)
            name .= w !! 7
        )
        zatom

-- * Write

-- | Write molecule to *.xyz.
writeXyz :: FilePath -> String -> Molecule -> IO ()
writeXyz ouf comment mol = do
  (tmp_name, tmp_handle) <- openTempFile "." "temp"
  hPrint tmp_handle (mol ^. atoms . to length)
  hPutStrLn tmp_handle comment
  mapM_ (writeData tmp_handle) (mol ^. atoms)
  hClose tmp_handle
  renameFile tmp_name ouf
  where
    writeData hdl i = do
      let e = i ^. aelement
          V3 x y z = i ^. acoords
      hPrintf hdl "%s\t%8.6f\t%8.6f\t%8.6f\n" e x y z

-- | Write molecule to *.pdb.
writePdb :: FilePath -> Molecule -> IO ()
writePdb ouf molecule = do
  (tmp_name, tmp_handle) <- openTempFile "." "temp"
  mapM_ (writeAtom tmp_handle) (M.toList (molecule ^. atoms))
  hClose tmp_handle
  renameFile tmp_name ouf
  where
    writeAtom hdl (i, a) =
      let a0 = a ^. adatatype
          a1 = i
          a2 = a ^. aname
          a3 = a ^. aaltloc
          a4 = a ^. aresname
          a5 = a ^. achainid
          a6 = a ^. aresseq
          a7 = a ^. arescode
          a8 = a ^. (acoords . _x)
          a9 = a ^. (acoords . _y)
          a10 = a ^. (acoords . _z)
          a11 = a ^. aoccup
          a12 = a ^. atempfac
          a13 = a ^. aelement
          a14 = a ^. acharge
          pat = "%-6s%5d %-4s%1s%-4s%1s%4d%1s   %8.3f%8.3f%8.3f%6.2f%6.2f\n"
       in hPrintf hdl pat a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12

-- -- | Write bonds
-- writeBonds ::
--      FilePath
--   -> [(Atom, Atom)]
--   -> IO ()
-- writeBonds ouf ab = do
--     let ab_str = map (\((i, _), (j, _)) ->
--                   show i ++ " " ++ show j) ab
--     (tmp_name, tmp_handle) <- openTempFile "." "temp"
--     mapM_ (hPutStrLn tmp_handle) ab_str
--     hClose tmp_handle
--     renameFile tmp_name ouf
