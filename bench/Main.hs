{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import           Codec.Encoding.UTF8

import           Data.Attoparsec.UTF8
import           Data.Text.UTF8
import           Generators

import           Control.Exception
import           Data.Attoparsec.ByteString.Lazy
import           Data.Bifunctor
import qualified Data.ByteString.Lazy as BSL
import           Data.Text.Encoding.Error
import           Data.Text.Lazy.Encoding as Lazy
import           Gauge
import           System.Entropy as Entropy
import           System.Random



deriving instance Show Error



strictAttoparsecB :: BSL.ByteString -> [Benchmark]
strictAttoparsecB input =
  [ bench "Hoehrmann" $
      whnf (parse . hoehrmannP $ HoehrmannHandler (Die "no")) input

  , bench "Lazy" $
      whnf (parse . naiveP $ NaiveHandler (\_ -> Die "no")) input
  ]



lenientAttoparsecB :: BSL.ByteString -> [Benchmark]
lenientAttoparsecB input =
  [ bench "Hoehrmann" $
      whnf (parse . hoehrmannP $ HoehrmannHandler (Replace '\xFFFD')) input

  , bench "Lazy" $
      whnf (parse . naiveP $ NaiveHandler (\_ -> Replace '\xFFFD')) input
  ]



strictTextB :: BSL.ByteString -> [Benchmark]
strictTextB input =
  [ bench "Hoehrmann with SIMD" $
      nf (Lazy.decodeUtf8With strictDecode) input

  , bench "Lazy with SIMD" $
      nf (first (const ()) . decodeSimdLazy (NaiveHandler $ \_ -> Die "no")) input

  , bench "Lazy" $
      nf (first (const ()) . decodeLazy (NaiveHandler $ \_ -> Die "no")) input
  ]



lenientTextB :: BSL.ByteString -> [Benchmark]
lenientTextB input =
  [ bench "Hoehrmann with SIMD" $
      nf (Lazy.decodeUtf8With lenientDecode) input

  , bench "Lazy with SIMD" $
      nf (first (const ()) . decodeSimdLazy (NaiveHandler $ \_ -> Replace '\xFFFD')) input

  , bench "Lazy" $
      nf (first (const ()) . decodeLazy (NaiveHandler $ \_ -> Replace '\xFFFD')) input
  ]



main :: IO ()
main =
  defaultMain
    [ env
        ( do g0 <- initStdGen
             let (correct_32KiB , g1) = lazyUtf8 g0 4096 8
                 (earlyErr_32KiB, g2) = lazySingleEarlyErrors g1 4096 8
                 (lateErr_32KiB , g3) = lazySingleLateErrors  g2 4096 8
                 (correct_2MiB  , g4) = lazyUtf8 g3 4096 512
                 (earlyErr_2MiB , g5) = lazySingleEarlyErrors g4 4096 512
                 (lateErr_2MiB  , _g) = lazySingleLateErrors g5 4096 512

             (garbage_32KiB, garbage_2MiB) <-
               bracket Entropy.openHandle Entropy.closeHandle $ \h -> do
                 small <- lazyEntropy h 4096 8
                 large <- lazyEntropy h 4096 512
                 pure (small, large)

             pure ( correct_32KiB
                  , earlyErr_32KiB
                  , lateErr_32KiB
                  , garbage_32KiB
                  , correct_2MiB
                  , earlyErr_2MiB
                  , lateErr_2MiB
                  , garbage_2MiB
                  )
        )
        $ \ ~( correct_32KiB
             , earlyErr_32KiB
             , lateErr_32KiB
             , garbage_32KiB
             , correct_2MiB
             , earlyErr_2MiB
             , lateErr_2MiB
             , garbage_2MiB
             ) ->

           bgroup "Bench"
             [ bgroup "Attoparsec"
                 [ bgroup "Correct 32KiB" $
                     strictAttoparsecB correct_32KiB

                 , bgroup "Correct 2MiB" $
                     strictAttoparsecB correct_2MiB

                 , bgroup "Early single errors 32KiB" $
                     lenientAttoparsecB earlyErr_32KiB

                 , bgroup "Early single errors 2MiB" $
                     lenientAttoparsecB earlyErr_2MiB

                 , bgroup "Late single errors 32KiB" $
                     lenientAttoparsecB lateErr_32KiB

                 , bgroup "Late single errors 2MiB" $
                     lenientAttoparsecB lateErr_2MiB

                 , bgroup "Garbage 32KiB" $
                     lenientAttoparsecB garbage_32KiB

                 , bgroup "Garbage 2MiB" $
                     lenientAttoparsecB garbage_2MiB
                 ]

             , bgroup "Text"
                 [ bgroup "Correct 32KiB" $
                     strictTextB correct_32KiB

                 , bgroup "Correct 2MiB" $
                     strictTextB correct_2MiB

                 , bgroup "Early single errors 32KiB" $
                     lenientTextB earlyErr_32KiB

                 , bgroup "Early single errors 2MiB" $
                     lenientTextB earlyErr_2MiB

                 , bgroup "Late single errors 32KiB" $
                     lenientTextB lateErr_32KiB

                 , bgroup "Late single errors 2MiB" $
                     lenientTextB lateErr_2MiB

                 , bgroup "Garbage 32KiB" $
                     lenientTextB garbage_32KiB

                 , bgroup "Garbage 2MiB" $
                     lenientTextB garbage_2MiB
                 ]
             ]
    ]
