module Codec.Encoding.UTF8 where

import           Data.Bits
import           Data.Word
import           GHC.Base



-- | Invalid character sequences that may be encountered when parsing UTF-8.
data Error = -- | Byte 1 is @10xxxxxx@
             Continuation Word8

             -- | Byte 1 is @11111xxx@
           | Invalid Word8

             -- | Byte 1 is @1110__1101__@, byte 2 is @10__1__xxxxx@
           | Surrogate Word8

             -- | Byte 1 is @110__0000__x@
           | Overlong2 Word8

             -- | Byte 1 is @1110__0000__@, byte 2 is @01__0__xxxxx@
           | Overlong3 Word8

             -- | Byte 1 is @11110__000__@, byte 2 is @01__00__xxxx@
           | Overlong4 Word8

             -- | Byte 1 is @110xxxxx@, byte 2 is not @10xxxxx@.
           | Incomplete22 Word8 Word8

             -- | Byte 1 is @1110xxxx@, byte 2 is not @10xxxxx@.
           | Incomplete23 Word8 Word8

             -- | Byte 1 is @11110xxx@, byte 2 is not @10xxxxx@.
           | Incomplete24 Word8 Word8

             -- | Byte 1 is @1110xxxx@, byte 3 is not @10xxxxx@
           | Incomplete33 Word8 Word8 Word8

             -- | Byte 1 is @11110xxx@, byte 3 is not @10xxxxx@
           | Incomplete34 Word8 Word8 Word8

             -- | Byte 1 is @11110xxx@, byte 4 is not @10xxxxx@.
           | Incomplete4 Word8 Word8 Word8 Word8

             -- | Byte 1 is @11110__1__xx@ and either of the @x@es is not 0.
           | Overflow1 Word8

             -- | Byte 1 is @11110__100__@, byte 2 is @10__00__xxxx@
           | Overflow2 Word8



newtype Conv1 a = Conv1 (Word8 -> a)

{-# INLINE char1 #-}
char1 :: Conv1 Char
char1 = Conv1 $ unsafeChr . fromIntegral

newtype Conv2 a = Conv2 (Word8 -> Word8 -> a)

{-# INLINE char2 #-}
char2 :: Conv2 Char
char2 = Conv2 $ \w0 w1 ->
          unsafeChr $ unsafeShiftL (fromIntegral w0 .&. 0x1F) 6
                    +              (fromIntegral w1 .&. 0x3F)

newtype Conv3 a = Conv3 (Word8 -> Word8 -> Word8 -> a)

{-# INLINE char3 #-}
char3 :: Conv3 Char
char3 = Conv3 $ \w0 w1 w2 ->
          unsafeChr $ unsafeShiftL (fromIntegral (w0 .&. 0x0F)) 12
                    + unsafeShiftL (fromIntegral (w1 .&. 0x3F)) 6
                    +              (fromIntegral (w2 .&. 0x3F))

newtype Conv4 a = Conv4 (Word8 -> Word8 -> Word8 -> Word8 -> a)

{-# INLINE char4 #-}
char4 :: Conv4 Char
char4 = Conv4 $ \w0 w1 w2 w3 ->
          unsafeChr $ unsafeShiftL (fromIntegral (w0 .&. 0x07)) 18
                    + unsafeShiftL (fromIntegral (w1 .&. 0x3F)) 12
                    + unsafeShiftL (fromIntegral (w2 .&. 0x3F))  6
                    +              (fromIntegral (w3 .&. 0x3F))



data UTF8 a = UTF8_1 a
            | Part_2 (Word8 -> UTF8_2 a)
            | Part_3_1 (Word8 -> Part_3_1 a)
            | Part_4_1 (Word8 -> Part_4_1 a)
            | Error_1 Error


data UTF8_2 a = UTF8_2 a
              | Error_2 Error


data Part_3_1 a = Part_3_2 (Word8 -> UTF8_3 a)
                | Error_3_1 Error

data UTF8_3 a = UTF8_3 a
              | Error_3_2 Error


data Part_4_1 a = Part_4_2 (Word8 -> Part_4_2 a)
                | Error_4_1 Error

data Part_4_2 a = Part_4_3 (Word8 -> UTF8_4 a)
                | Error_4_2 Error

data UTF8_4 a = UTF8_4 a
              | Error_4_3 Error



{-# INLINE utf8 #-}
utf8 :: Conv1 a -> Conv2 a -> Conv3 a -> Conv4 a -> Word8 -> UTF8 a
utf8 (Conv1 conv1) (Conv2 conv2) (Conv3 conv3) (Conv4 conv4) w0
  | (w0 .&. 0x80) == 0 = UTF8_1 (conv1 w0)

  | (w0 .&. 0x40) == 0 = Error_1 $ Continuation w0

  | (w0 .&. 0x20) == 0 =
      if (w0 .&. 0x1F) < 0x02
        then Error_1 $ Overlong2 w0
        else Part_2 $ \w1 ->
               if (w1 .&. 0xC0) /= 0x80
                 then Error_2 $ Incomplete22 w0 w1
                 else UTF8_2 $ conv2 w0 w1

  | (w0 .&. 0x10) == 0 =
      Part_3_1 $ \w1 ->
        if (w1 .&. 0xC0) /= 0x80
          then Error_3_1 $ Incomplete23 w0 w1
          else
            if (w0 .&. 0x0F) == 0x0D && (w1 .&. 0x20) /= 0
              then Error_3_1 $ Surrogate w1
              else
                if (w0 .&. 0x0F) == 0 && (w1 .&. 0x20) == 0
                  then Error_3_1 $ Overlong3 w1
                  else Part_3_2 $ \w2 ->
                        if (w2 .&. 0xC0) /= 0x80
                          then Error_3_2 $ Incomplete33 w0 w1 w2
                          else UTF8_3 $ conv3 w0 w1 w2

  | (w0 .&. 0x08) == 0 =
      if (w0 .&. 0x07) > 0x04
        then Error_1 $ Overflow1 w0
        else Part_4_1 $ \w1 ->
               if (w1 .&. 0xC0) /= 0x80
                 then Error_4_1 $ Incomplete24 w0 w1
                 else
                   if (w0 .&. 0x07) == 0x04 && (w1 .&. 0x3F) >= 0x10
                     then Error_4_1 $ Overflow2 w1
                     else
                       if (w0 .&. 0x07) == 0x00 && (w1 .&. 0x3F) < 0x10
                         then Error_4_1 $ Overlong4 w1
                         else Part_4_2 $ \w2 ->
                                if (w2 .&. 0xC0) /= 0x80
                                  then Error_4_2 $ Incomplete34 w0 w1 w2
                                  else Part_4_3 $ \w3 ->
                                         if (w3 .&. 0xC0) /= 0x80
                                           then Error_4_3 $ Incomplete4 w0 w1 w2 w3
                                           else UTF8_4 $ conv4 w0 w1 w2 w3

  | otherwise         = Error_1 $ Invalid w0
