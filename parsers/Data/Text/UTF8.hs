{-# LANGUAGE BangPatterns
           , Rank2Types #-}

module Data.Text.UTF8
  ( NaiveHandler (..)

  , decodeSimdStrict
  , decodeSimdLazy

  , decodeStrict
  , decodeLazy
  ) where

import           Codec.Encoding.UTF8
import           Data.Text.Copy
import           Parser.Handler

import           Data.Bits
import qualified Data.ByteString as BS
import           Data.ByteString.Internal (ByteString (..), unsafeWithForeignPtr)
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Internal as BSL (ByteString (..))
import           Data.Text.Internal (Text (..))
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Internal.Lazy as Lazy (Text (..))
import           Data.Word
import           Foreign.C.Types
import           Foreign.Ptr
import           System.IO.Unsafe



data Resume = Resume_2_2 Word8 (Word8 -> UTF8_2 Char)
            | Resume_2_3 Word8 (Word8 -> Part_3_1 Char)
            | Resume_2_4 Word8 (Word8 -> Part_4_1 Char)

            | Resume_3_3 Word8 Word8 (Word8 -> UTF8_3 Char)
            | Resume_3_4 Word8 Word8 (Word8 -> Part_4_2 Char)

            | Resume_4 Word8 Word8 Word8 (Word8 -> UTF8_4 Char)

data Decoded = Empty
             | Decoded Copy (Maybe Resume)
             | Failure Int Error



foreign import ccall unsafe "_hs_text_is_valid_utf8" c_is_valid_utf8
 :: Ptr Word8 -> CSize -> IO CInt

{-# INLINE validateSimd #-}
validateSimd :: ByteString -> Int -> Int
validateSimd bs n =
  let len = BS.length bs
      i0 = len - 1
      i1 = i0 - 1
      i2 = i1 - 1
      i3 = i2 - 1

      nonCont u = (u .&. 0x80) == 0 || (u .&. 0x40) /= 0

  in if len - n < 4
       then n
       else
         if nonCont $ BS.unsafeIndex bs i0
           then validate i0
           else
             if nonCont $ BS.unsafeIndex bs i1
               then validate i1
               else
                 if nonCont $ BS.unsafeIndex bs i2
                   then validate i2
                   else
                     if nonCont $ BS.unsafeIndex bs i3
                       then validate i3
                       else fallback

  where
    {-# NOINLINE fallback #-}
    fallback = validateChunk bs n

    validate i =
      let BS fptr _len = BS.unsafeDrop n bs

          advance x =
              case x of
                1 -> i
                _ -> fallback

      in advance . unsafeDupablePerformIO $ do
                     unsafeWithForeignPtr fptr $ \ptr ->
                       c_is_valid_utf8 ptr (fromIntegral $ i - n)



{-# INLINE validateChunk #-}
-- | 'ByteString' must be non-empty.
validateChunk :: ByteString -> Int -> Int
validateChunk bs = go
  where
    {-# NOINLINE len #-}
    len = BS.length bs

    go n0
      | n0 >= len = n0
      | otherwise =
          let n1 = n0 + 1
              n2 = n1 + 1
              n3 = n2 + 1
              n4 = n3 + 1

              u0 = BS.unsafeIndex bs n0
              u1 = BS.unsafeIndex bs n1
              u2 = BS.unsafeIndex bs n2
              u3 = BS.unsafeIndex bs n3

          in case utf8 char1 char2 char3 char4 u0 of
               UTF8_1 _  -> go n1

               Part_2 f0 ->
                 if n1 >= len
                   then n0
                   else case f0 u1 of
                          UTF8_2 _  -> go n2
                          Error_2 _ -> n0

               Part_3_1 f0 ->
                 if n1 >= len
                   then n0
                   else case f0 u1 of
                          Part_3_2 f1 ->
                            if n2 >= len
                              then n0
                              else case f1 u2 of
                                     UTF8_3 _    -> go n3
                                     Error_3_2 _ -> n0

                          Error_3_1 _ -> n0

               Part_4_1 f0 ->
                 if n1 >= len
                   then n0
                   else case f0 u1 of
                          Part_4_2 f1 ->
                            if n2 >= len
                              then n0
                              else case f1 u2 of
                                     Part_4_3 f2 ->
                                       if n3 >= len
                                         then n0
                                         else case f2 u3 of
                                                UTF8_4 _    -> go n4
                                                Error_4_3 _ -> n0

                                     Error_4_2 _ -> n0

                          Error_4_1 _ -> n0

               Error_1 _ -> n0



{-# INLINE decodeChunk #-}
-- | 'ByteString' must be non-empty.
decodeChunk :: (ByteString -> Int -> Int) -> NaiveHandler -> ByteString -> Maybe Resume -> Decoded
decodeChunk validate (NaiveHandler handler) bs mayResume =
  case mayResume of
    Nothing     -> fast emptyCopy 0
    Just resume ->
      case resume of
        Resume_2_2 u0 f0 -> slow_2_2 emptyCopy 0 u0 f0
        Resume_2_3 u0 f0 -> slow_2_3 emptyCopy 0 u0 f0
        Resume_2_4 u0 f0 -> slow_2_4 emptyCopy 0 u0 f0

        Resume_3_3 u0 u1 f1 -> slow_3_3 emptyCopy 0 u0 u1 f1
        Resume_3_4 u0 u1 f1 -> slow_3_4 emptyCopy 0 u0 u1 f1

        Resume_4 u0 u1 u2 f2 -> slow_4 emptyCopy 0 u0 u1 u2 f2

  where
    {-# NOINLINE len #-}
    len = BS.length bs

    {-# INLINE recover #-}
    recover copy n e = case handler e of
                         Die _     -> Failure n e
                         Replace c -> if n >= len
                                        then Decoded (writeCopyChar copy c) Nothing
                                        else slow (writeCopyChar copy c) n
                         Ignore    -> if n >= len
                                        then Decoded copy Nothing
                                        else slow copy n

    {-# INLINE slow_2_2 #-}
    slow_2_2 copy n1 u0 f0 =
      let n2 = n1 + 1
          u1 = BS.unsafeIndex bs n1

      in case f0 u1 of
           UTF8_2 _  -> fast (writeCopy2 copy u0 u1) n2
           Error_2 e -> recover copy n1 e


    {-# INLINE slow_2_3 #-}
    slow_2_3 copy n1 u0 f0 =
      let n2 = n1 + 1
          u1 = BS.unsafeIndex bs n1

      in case f0 u1 of
           Part_3_2 f1 ->
             if n2 >= len
               then Decoded copy . Just $ Resume_3_3 u0 u1 f1
               else slow_3_3 copy n2 u0 u1 f1

           Error_3_1 e -> recover copy n1 e

    {-# INLINE slow_3_3 #-}
    slow_3_3 copy n2 u0 u1 f1 =
      let n3 = n2 + 1
          u2 = BS.unsafeIndex bs n2

      in case f1 u2 of
           UTF8_3 _    -> fast (writeCopy3 copy u0 u1 u2) n3
           Error_3_2 e -> recover copy n2 e


    {-# INLINE slow_2_4 #-}
    slow_2_4 copy n1 u0 f0 =
      let n2 = n1 + 1
          u1 = BS.unsafeIndex bs n1

      in case f0 u1 of
           Part_4_2 f1 ->
             if n2 >= len
               then Decoded copy . Just $ Resume_3_4 u0 u1 f1
               else slow_3_4 copy n2 u0 u1 f1

           Error_4_1 e -> recover copy n1 e

    {-# INLINE slow_3_4 #-}
    slow_3_4 copy n2 u0 u1 f1 =
      let n3 = n2 + 1
          u2 = BS.unsafeIndex bs n2

      in case f1 u2 of
           Part_4_3 f2 ->
             if n3 >= len
               then Decoded copy . Just $ Resume_4 u0 u1 u2 f2
               else slow_4 copy n3 u0 u1 u2 f2

           Error_4_2 e -> recover copy n2 e

    {-# INLINE slow_4 #-}
    slow_4 !copy n3 u0 u1 u2 f2 =
      let n4 = n3 + 1
          u3 = BS.unsafeIndex bs n3

      in case f2 u3 of
           UTF8_4 _    -> fast (writeCopy4 copy u0 u1 u2 u3) n4
           Error_4_3 e -> recover copy n3 e


    {-# NOINLINE fast #-}
    fast !copy n0
      | n0 >= len = Decoded copy Nothing
      | otherwise =
          let n' = validate bs n0
              copyBS = writeCopyByteString copy $ BS.unsafeDrop n0 (BS.unsafeTake n' bs)
          in if n' >= len
               then Decoded copyBS Nothing
               else if n' > n0
                      then slow copyBS n'
                      else slow copy n0

    {-# NOINLINE slow #-}
    slow !copy n0 =
      let n1 = n0 + 1
          u0 = BS.unsafeIndex bs n0

      in case utf8 char1 char2 char3 char4 u0 of
           UTF8_1 _  -> fast (writeCopy1 copy u0) n1

           Part_2 f0 ->
             if n1 >= len
               then Decoded copy . Just $ Resume_2_2 u0 f0
               else slow_2_2 copy n1 u0 f0

           Part_3_1 f0 ->
             if n1 >= len
               then Decoded copy . Just $ Resume_2_3 u0 f0
               else slow_2_3 copy n1 u0 f0

           Part_4_1 f0 ->
             if n1 >= len
               then Decoded copy . Just $ Resume_2_4 u0 f0
               else slow_2_4 copy n1 u0 f0

           Error_1 e -> recover copy n1 e



{-# INLINE decodeStrict_ #-}
decodeStrict_ :: (ByteString -> Int -> Int) -> NaiveHandler -> ByteString -> Either (Int, Error) Text
decodeStrict_ validate handler bs
  | BS.null bs = Right Text.empty
  | otherwise  =
      case decodeChunk validate handler bs Nothing of
        Decoded copy mayResume ->
          case mayResume of
            Nothing -> let txt = commitCopy copy
                       in txt `seq` Right txt

            Just resume -> handleTrail handler (BS.length bs) copy resume

        Failure n e -> Left (n, e)

        Empty -> Right Text.empty



{-# INLINE decodeSimdStrict #-}
decodeSimdStrict :: NaiveHandler -> ByteString -> Either (Int, Error) Text
decodeSimdStrict = decodeStrict_ validateSimd

{-# INLINE decodeStrict #-}
decodeStrict :: NaiveHandler -> ByteString -> Either (Int, Error) Text
decodeStrict = decodeStrict_ validateChunk



handleTrail :: NaiveHandler -> Int -> Copy -> Resume -> Either (Int, Error) Text
handleTrail (NaiveHandler handler) len copy resume =
  let e = case resume of
            Resume_2_2 u0 _ -> Incomplete22 u0 0
            Resume_2_3 u0 _ -> Incomplete23 u0 0
            Resume_2_4 u0 _ -> Incomplete24 u0 0

            Resume_3_3 u0 u1 _ -> Incomplete33 u0 u1 0
            Resume_3_4 u0 u1 _ -> Incomplete34 u0 u1 0

            Resume_4 u0 u1 u2 _ -> Incomplete4 u0 u1 u2 0

  in case handler e of
       Die _ -> Left (len, e)

       Replace c -> let txt = commitCopy (writeCopyChar copy c)
                    in txt `seq` Right txt

       Ignore -> let txt = commitCopy copy
                 in txt `seq` Right txt



{-# INLINE decodeLazy_ #-}
decodeLazy_ :: (ByteString -> Int -> Int) -> NaiveHandler -> BSL.ByteString -> Either (Int, Error) Lazy.Text
decodeLazy_ validate handler = go 0 id Nothing
  where
    go !len acc mayResume (BSL.Chunk bs rest) =
      case decodeChunk validate handler bs mayResume of
        Decoded copy mayResume' ->
          let txt = commitCopy copy
          in txt `seq` go (len + BS.length bs) (acc . Lazy.Chunk txt) mayResume' rest

        Failure n e -> Left (n, e)

        Empty -> Right $ acc Lazy.empty

    go len acc mayResume BSL.Empty =
      case mayResume of
        Nothing     -> Right $ acc Lazy.empty
        Just resume -> acc . Lazy.fromStrict <$> handleTrail handler len emptyCopy resume



{-# INLINE decodeSimdLazy #-}
decodeSimdLazy :: NaiveHandler -> BSL.ByteString -> Either (Int, Error) Lazy.Text
decodeSimdLazy = decodeLazy_ validateSimd

{-# INLINE decodeLazy #-}
decodeLazy :: NaiveHandler -> BSL.ByteString -> Either (Int, Error) Lazy.Text
decodeLazy = decodeLazy_ validateChunk
