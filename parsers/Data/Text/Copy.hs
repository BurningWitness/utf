{-# LANGUAGE RankNTypes #-}

module Data.Text.Copy
  ( Copy (..)

  , emptyCopy

  , writeCopy
  , writeCopy1
  , writeCopy2
  , writeCopy3
  , writeCopy4
  , writeCopyChar
  , writeCopyByteString

  , commitCopy
  ) where

import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Data.Bits
import           Data.ByteString.Internal (ByteString (..))
import           Data.Char (ord)
import           Data.Text.Array
import           Data.Text.Internal (Text (..))
import           Data.Word
import           GHC.ForeignPtr



data Copy = Copy Int (forall s. MArray s -> Int -> ST s ())

{-# INLINE emptyCopy #-}
emptyCopy :: Copy
emptyCopy = Copy 0 $ \_ _ -> pure ()

{-# INLINE writeCopy #-}
writeCopy :: Copy -> Int -> (forall s. MArray s -> Int -> ST s ()) -> Copy
writeCopy (Copy off writes) n put =
  Copy (off + n) $ \ptr ffo -> do let ffo' = ffo - n
                                  put ptr ffo'
                                  writes ptr ffo'



{-# INLINE commitCopy #-}
commitCopy :: Copy -> Text
commitCopy (Copy off writes) =
  runST $ do
    marr <- new off
    writes marr off
    arr <- unsafeFreeze marr
    pure (Text arr 0 off)



{-# INLINE writeCopy1 #-}
writeCopy1 :: Copy -> Word8 -> Copy
writeCopy1 copy u0 =
  writeCopy copy 1 $ \marr off -> unsafeWrite marr off u0


{-# INLINE writeCopy2 #-}
writeCopy2 :: Copy -> Word8 -> Word8 -> Copy
writeCopy2 copy u0 u1 =
  writeCopy copy 2 $ \marr off -> do unsafeWrite marr  off      u0
                                     unsafeWrite marr (off + 1) u1

{-# INLINE writeCopy3 #-}
writeCopy3 :: Copy -> Word8 -> Word8 -> Word8 -> Copy
writeCopy3 copy u0 u1 u2 =
  writeCopy copy 3 $ \marr off -> do unsafeWrite marr  off      u0
                                     unsafeWrite marr (off + 1) u1
                                     unsafeWrite marr (off + 2) u2

{-# INLINE writeCopy4 #-}
writeCopy4 :: Copy -> Word8 -> Word8 -> Word8 -> Word8 -> Copy
writeCopy4 copy u0 u1 u2 u3 =
  writeCopy copy 4 $ \marr off -> do unsafeWrite marr  off      u0
                                     unsafeWrite marr (off + 1) u1
                                     unsafeWrite marr (off + 2) u2
                                     unsafeWrite marr (off + 3) u3

{-# INLINE writeCopyChar #-}
writeCopyChar :: Copy -> Char -> Copy
writeCopyChar copy char =
  let i = ord char
  in if i < 0x80
       then writeCopy1 copy (fromIntegral i)
       else
         if i < 0x800
           then writeCopy2 copy
                  (0xC0 + (fromIntegral $ i `unsafeShiftR` 6))
                  (0x80 + (fromIntegral i .&. 0x3F))

           else
             if i <= 0xFFFF
               then writeCopy3 copy
                      (0xE0 + (fromIntegral $ i `unsafeShiftR` 12))
                      (0x80 + (fromIntegral $ i `unsafeShiftR` 6) .&. 0x3F)
                      (0x80 + (fromIntegral i .&. 0x3F))

               else
                 if i <= 0x10FFFF
                   then writeCopy4 copy
                          (0xF0 + (fromIntegral $ i `unsafeShiftR` 18))
                          (0x80 + (fromIntegral $ i `unsafeShiftR` 12) .&. 0x3F)
                          (0x80 + (fromIntegral $ i `unsafeShiftR` 6) .&. 0x3F)
                          (0x80 + (fromIntegral i .&. 0x3F))

                   else writeCopy3 copy 0xEF 0xBF 0xBD

{-# INLINE writeCopyByteString #-}
writeCopyByteString :: Copy -> ByteString -> Copy
writeCopyByteString copy (BS fptr len) =
  writeCopy copy len $ \marr off ->
    unsafeIOToST $
      unsafeWithForeignPtr fptr $ \ptr ->
        unsafeSTToIO $
          copyFromPointer marr off ptr len
