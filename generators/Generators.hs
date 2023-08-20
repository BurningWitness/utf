module Generators
  ( lazyUtf8

  , lazySingleEarlyErrors
  , lazySingleLateErrors

  , lazyEntropy
  ) where

import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.ByteString.Lazy.Internal as BSL (chunk)
import           Data.Char
import           Data.Word
import           GHC.Base
import           System.Entropy as Entropy
import           System.Random



mkUtf8 :: Char -> (Int, [Word8])
mkUtf8 c =
  let i = ord c
  in case () of
       () | i < 0x80  ->
              (,) 1 [fromIntegral i]

          | i < 0x800 ->
              (,) 2 [ 0xC0 + fromIntegral (i `unsafeShiftR` 6)
                    , 0x80 + fromIntegral (i .&. 0x3F)
                    ]

          | i <= 0xFFFF ->
              (,) 3 [ 0xE0 + fromIntegral (i `unsafeShiftR` 12)
                    , 0x80 + fromIntegral ((i `unsafeShiftR` 6) .&. 0x3F)
                    , 0x80 + fromIntegral (i .&. 0x3F)
                    ]

          | i <= 0x10FFFF ->
              (,) 4 [ 0xF0 + fromIntegral (i `unsafeShiftR` 18)
                    , 0x80 + fromIntegral ((i `unsafeShiftR` 12) .&. 0x3F)
                    , 0x80 + fromIntegral ((i `unsafeShiftR` 6) .&. 0x3F)
                    , 0x80 + fromIntegral (i .&. 0x3F)
                    ]

          | otherwise -> errorWithoutStackTrace $ "utf8Poke: " <> show i



char1 :: RandomGen g => g -> (Char, g)
char1 = uniformR ('\x0', '\x7F')

char2 :: RandomGen g => g -> (Char, g)
char2 = uniformR ('\x80', '\x7FF')

char3 :: RandomGen g => g -> (Char, g)
char3 g = let ~(c, g') = uniformR (0x800, 0xF7FF) g
              c' = if c < 0xD800
                     then c
                     else c + 0x800

          in (unsafeChr c', g')

char4 :: RandomGen g => g -> (Char, g)
char4 = uniformR ('\x10000', '\x10FFFF')



char :: RandomGen g => g -> (Char, g)
char g =
  let ~(b, g') = uniform g
  in case b .&. 0x3 :: Word of
       0  -> char1 g'
       1  -> char2 g'
       2  -> char3 g'
       _3 -> char4 g'



carry :: RandomGen g => g -> Int -> ([Word8], [Word8], g)
carry = go
  where
    go g n =
      let ~(c, g') = char g
          ~(i, ws) = mkUtf8 c

      in if i >= n
           then (take n ws, drop n ws, g')
           else (\ ~(x, y, z) -> (ws <> x, y, z) ) $ go g' (n - i)



noCarry :: RandomGen g => g -> Int -> ([Word8], g)
noCarry = go
  where
    go g n =
      let ~(c, g') = char g
          ~(i, ws) = mkUtf8 c

      in if i >= n
           then case n of
                  3 -> let ~(c3, g'') = char3 g'
                           ~(_ , ws3) = mkUtf8 c3

                       in (ws3, g'')

                  2 -> let ~(c2, g'') = char2 g'
                           ~(_ , ws2) = mkUtf8 c2

                       in (ws2, g'')

                  1 -> let ~(c1, g'') = char1 g'
                           ~(_ , ws1) = mkUtf8 c1

                       in (ws1, g'')

                  _ -> ([], g')

           else (\ ~(x, z) -> (ws <> x, z) ) $ go g' (n - i)



-- | Generates valid UTF8.
--
--   First 'Int' is chunk size in bytes, second 'Int' is number of chunks.
lazyUtf8 :: RandomGen g => g -> Int -> Int -> (BSL.ByteString, g)
lazyUtf8 g len n =
  let ~(ws, g') = noCarry g (len * n)
  in (go ws n, g')
  where
    go ws m
      | m <= 0    = BSL.empty
      | otherwise = BSL.chunk (BS.pack $ take len ws) $ go (drop len ws) (m - 1)



-- | Generates mostly valid UTF8, putting a single @0xFF@ character
--   0-3 bytes deep into every chunk.
--
--   First 'Int' is chunk size in bytes, second 'Int' is number of chunks.
--
--   This always generates chunks at least @1-5@ bytes large.
lazySingleEarlyErrors :: RandomGen g => g -> Int -> Int -> (BSL.ByteString, g)
lazySingleEarlyErrors g0 len = go g0 []
  where
    go g cs n
      | n <= 1    =
         if n == 1
           then let ~(ws, g') = noCarry g (len - length cs - 1)
                in (BSL.fromStrict (BS.pack $ cs <> (0xFF:ws)), g')

           else (BSL.empty, g)

      | otherwise =
          let ~(ws, cs', g') = carry g (len - length cs - 1)
          in ( \ ~(bsl, g'') -> (BSL.chunk (BS.pack $ cs <> (0xFF:ws)) bsl, g''))
               $ go g' cs' (n - 1)



-- | Generates mostly valid UTF8, putting a single @0xFF@ character
--   5-8 bytes deep from the end of every chunk.
--
--   This always generates chunks at least @4-12@ bytes large.
--
--   First 'Int' is chunk size in bytes, second 'Int' is number of chunks.
lazySingleLateErrors :: RandomGen g => g -> Int -> Int -> (BSL.ByteString, g)
lazySingleLateErrors g0 len = go g0 []
  where
    go g cs n
      | n <= 1    =
         if n == 1
           then let ~(ws, ys, g') = carry g (len - length cs - 9)
                    ~(xs, g'') = noCarry g' (8 - length ys)
                in (BSL.fromStrict (BS.pack $ cs <> ws <> ys <> (0xFF:xs)), g'')

           else (BSL.empty, g)

      | otherwise =
          let ~(ws, ys, g')   = carry g (len - length cs - 9)
              ~(xs, cs', g'') = carry g' (8 - length ys)
          in ( \ ~(bsl, g''') ->
                (BSL.chunk (BS.pack $ cs <> ws <> ys <> (0xFF:xs)) bsl, g''')
              )
               $ go g'' cs' (n - 1)



-- | Generates noise.
--
--   First 'Int' is chunk size in bytes, second 'Int' is number of chunks.
lazyEntropy :: CryptHandle -> Int -> Int -> IO BSL.ByteString
lazyEntropy h len = go
  where
    go n
      | n <= 0    = pure BSL.empty
      | otherwise = do
          bs <- hGetEntropy h len
          BSL.chunk bs <$> go (n - 1)
