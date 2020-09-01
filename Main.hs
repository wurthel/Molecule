-- import Program.RemoveWater
-- import Program.SearchBonds
-- import Program.DistanceGeometry
import Program.InsertMolecule

import System.Environment
import Control.Lens
import Control.Monad
import Util.ReadWrite
import Type.Atom
import Type.Molecule
import Util.TrigDegree
import Data.List

import System.Directory
import System.IO.Unsafe (unsafePerformIO)
import System.IO

import Linear


tests = ["hard14", "sub1", "sub2", "sub3", "sub4", "sub5", "sub6", "sub7", "sub8", "sub91", "sub10", "sub11", "sub12", "sub13"]

main :: IO ()
main = writePdb "result.pdb" $ addAtom (8000, atom) $ readPdb "2Z73.pdb" 
    --print $ rotateAtom (V3 1 0 0) (Degree 90) atom -- removeH20 -- distGeom "sub12" -- mapM_ distGeom testsa

-- removeH20 :: IO ()
-- removeH20 = do
--   args <- getArgs
--   let ifn = args !! 0
--       ofn = args !! 1
--       cbs = read $ args !! 2 
--   writePdb ofn (removeWater cbs (readPdb ifn))

-- distGeom :: 
--      String
--   -> IO ()
-- distGeom mn = do
--   let num = 1
--       tol = 0.1
--       fnm = "Data/Input/DistanceGeometry/Tests/" ++ mn ++ ".mol"
--       odir = "Data/Results/DistanceGeometry/" ++ mn
--       ofn = odir ++ "/" ++ mn
  
--   let mol = readMolV2000 fnm
--   m <- distanceGeometry tol mol num
--   createDirectory odir
--   zipWithM_ (\x i -> do
--     let m = fst x
--         e = snd x
--         f = ofn ++ "-" ++ show i ++ ".xyz"
--         msg = "error: " ++ show e
--     print e
--     writeXyz f msg m) m [1..]

-- removeH = mapM_ removeH' [1..100] 
--   where
--   removeH' i = 
--     do
--     let dr = "Data/Results/DistanceGeometry/C6H12/Optimized"
--         fn = dr ++ "/C6H12-optimized-" ++ show i ++ ".xyz"
--         ouf = dr ++ "/Optimized-0/" ++ "C6H12-optimized-0-" ++ show i  ++ ".xyz"
--         txt = (lines . unsafePerformIO . readFile) fn
--     (tmp_name, tmp_handle) <- openTempFile "." "temp"
--     let txt' = filter (\x -> let w = words x in (not . null) w && head w == "C") txt
--     hPutStrLn tmp_handle (show $ length txt')
--     hPutStrLn tmp_handle ""
--     mapM_ (hPutStrLn tmp_handle) txt'
--     hClose tmp_handle
--     renameFile tmp_name ouf
  