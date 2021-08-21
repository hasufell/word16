{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import GHC.ST
    ( ST (ST), runST )
import Criterion.Main
import GHC.Exts
import GHC.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as S
import Data.ByteString.Short.Internal
import Data.Word16

asBA :: ShortByteString -> BA
asBA (SBS ba#) = BA# ba#

data BA = BA# ByteArray#
data MBA s = MBA# (MutableByteArray# s)


create :: Int -> (forall s. MBA s -> ST s ()) -> ShortByteString
create len fill =
    runST $ do
      mba <- newByteArray len
      fill mba
      BA# ba# <- unsafeFreezeByteArray mba
      return (SBS ba#)
{-# INLINE create #-}


main :: IO ()
main = do
    input <- S.toShort <$> BS.readFile "bench/Bench.hs"
    defaultMain [ 
        bench "Data.Word16" $ whnf (map' Data.Word16.toLower) input
      ]


map' :: (Word16 -> Word16) -> ShortByteString -> ShortByteString
map' f = \sbs ->
    let l = S.length sbs
        ba = asBA sbs
    in create (l * 2) (\mba -> go ba mba 0 l)
  where
    go :: BA -> MBA s -> Int -> Int -> ST s ()
    go !ba !mba !i !l
      | i >= l = return ()
      | otherwise = do
          let w = indexWord16Array ba i
          writeWord16Array mba i (f w)
          go ba mba (i+1) l
{-# INLINE map' #-}


writeWord16Array :: MBA s -> Int -> Word16 -> ST s ()
writeWord16Array (MBA# mba#) (I# i#) (W16# w#) =
  ST $ \s -> case writeWord16Array# mba# i# w# s of
               s' -> (# s', () #)

indexWord16Array :: BA -> Int -> Word16
indexWord16Array (BA# ba#) (I# i#) = W16# (indexWord16Array# ba# i#)

newByteArray :: Int -> ST s (MBA s)
newByteArray (I# len#) =
    ST $ \s -> case newByteArray# len# s of
                 (# s', mba# #) -> (# s', MBA# mba# #)

unsafeFreezeByteArray :: MBA s -> ST s BA
unsafeFreezeByteArray (MBA# mba#) =
    ST $ \s -> case unsafeFreezeByteArray# mba# s of
                 (# s', ba# #) -> (# s', BA# ba# #)
