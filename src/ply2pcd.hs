{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Applicative
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import Data.List (find, findIndex)
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import Data.Word (Word8, Word32)
import Linear
import qualified PLY
import qualified PLY.Types as PLY
import qualified PCD.Data as PCD
import qualified PCD.Header as PCD
import System.Environment
import Unsafe.Coerce (unsafeCoerce)

getElement :: ByteString -> PLY.Header -> Maybe PLY.Element
getElement n (_, els) = find ((==n) . PLY.elName) els

getPropIndex :: [PLY.Property] -> ByteString -> Maybe Int
getPropIndex props n = findIndex aux props
  where aux (PLY.ScalarProperty _ m) | CI.mk m == n' = True
        aux _ = False
        n' = CI.mk n

-- Get X,Y,Z and maybe RGB indices in the Element vector.
getProps :: PLY.Element -> (V3 Int, Maybe (V3 Int))
getProps e = (pos, col)
  where pos | all isJust reqd = let [x,y,z] = catMaybes reqd in V3 x y z
            | otherwise = error "Couldn't find vertex position properties"
        col | any isNothing opts = Nothing
            | otherwise = let [r,g,b] = catMaybes opts in Just $ V3 r g b
        reqd = map getProp ["x", "y", "z"]
        opts = map getProp ["red", "green", "blue"]
        getProp = getPropIndex (PLY.elProps e)

posPcd :: Vector (Vector PLY.Scalar) -> V3 Int
       -> (PCD.Header, S.Vector Float)
posPcd el posInds = (h,v)
  where n = V.length el
        h = PCD.mkSimpleHeader ["x","y","z"] (PCD.F, 4) n
        v = S.unsafeCast . S.convert $ 
            V.map (\e -> fmap (getProp e) posInds) el
        getProp e i = PLY.unsafeUnwrap $ e V.! i :: Float

colPcd :: Vector (Vector PLY.Scalar) -> V3 Int -> V3 Int
       -> (PCD.Header, S.Vector Float)
colPcd el posInds colInds = (h,v)
  where n = V.length el
        h = PCD.mkSimpleHeader ["x","y","z","rgb"] (PCD.F,4) n
        v = S.unsafeCast . S.convert $
            V.map (\e -> snoc (fmap (getPropf e) posInds)
                              (pack $ fmap (getPropb e) colInds))
                  el
        getPropf e i = PLY.unsafeUnwrap $ e V.! i :: Float
        getPropb e i = PLY.unsafeUnwrap $ e V.! i :: Word8
        snoc (V3 x y z) w = V4 x y z w
        pack (V3 r g b) = (unsafeCoerce::Word32->Float) $
                          shiftL (fromIntegral r) 24
                       .|. shiftL (fromIntegral b) 16
                       .|. shiftL (fromIntegral g) 8

ply2pcd :: FilePath -> FilePath -> IO ()
ply2pcd src dst = do Right plydata <- PLY.loadHeader src
                     let hd = PLY.plyHeader plydata
                         Right el = PLY.loadPlyElements "vertex" plydata
                         inds = getProps <$> getElement "vertex" hd
                         convertProp (pinds, cinds) = 
                           let (h,v) = maybe (posPcd el pinds)
                                             (colPcd el pinds)
                                             cinds
                           in PCD.saveBinaryPcd dst h v
                     maybe (error "Couldn't find \"vertex\" element!")
                           convertProp
                           inds
                                       
main :: IO ()
main = do args <- getArgs
          case args of
            [src,dst] -> ply2pcd src dst
            _ -> putStrLn "Usage: ply2pcd src.ply dst.pcd"
