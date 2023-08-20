module Data.Attoparsec.UTF8
  ( Action (..)

  , HoehrmannHandler (..)
  , hoehrmannP

  , NaiveHandler (..)
  , naiveP
  ) where

import           Codec.Encoding.UTF8
import           Parser.Handler

import           Data.Attoparsec.ByteString
import           Data.Text.Internal.Encoding.Utf8



newtype HoehrmannHandler = HoehrmannHandler Action

{-# INLINE hoehrmannP #-}
hoehrmannP :: HoehrmannHandler -> Parser String
hoehrmannP (HoehrmannHandler handler) = go
  where
    {-# INLINE recover #-}
    recover = case handler of
                Die msg   -> fail msg
                Replace c -> (:) c <$> go
                Ignore    -> go

    go = do
      mayw0 <- peekWord8
      case mayw0 of
        Just w0 -> do
          _ <- anyWord8
          case utf8DecodeStart w0 of
            Accept c         -> (:) c <$> go
            Incomplete s0 p0 -> do
              mayw1 <- peekWord8
              case mayw1 of
                Just w1 ->
                  case utf8DecodeContinue w1 s0 p0 of
                    Accept c         -> do _ <- anyWord8
                                           (:) c <$> go
                    Incomplete s1 p1 -> do
                      _ <- anyWord8
                      mayw2 <- peekWord8
                      case mayw2 of
                        Just w2 ->
                          case utf8DecodeContinue w2 s1 p1 of
                            Accept c         -> do _ <- anyWord8
                                                   (:) c <$> go
                            Incomplete s2 p2 -> do
                              _ <- anyWord8
                              mayw3 <- peekWord8
                              case mayw3 of
                                Just w3 ->
                                  case utf8DecodeContinue w3 s2 p2 of
                                    Accept c       -> do _ <- anyWord8
                                                         (:) c <$> go
                                    Incomplete _ _ -> recover
                                    Reject         -> recover

                                Nothing -> recover

                            Reject         -> recover

                        Nothing -> recover

                    Reject         -> recover

                Nothing -> recover

            Reject         -> recover

        Nothing -> pure []



{-# INLINE naiveP #-}
naiveP :: NaiveHandler -> Parser String
naiveP (NaiveHandler handler) = go
  where
    {-# INLINE recover #-}
    recover e = case handler e of
                  Die msg   -> fail msg
                  Replace c -> (:) c <$> go
                  Ignore    -> go

    {-# NOINLINE go #-}
    go = do
      mayw0 <- peekWord8
      case mayw0 of
        Just w0 -> do
          _ <- anyWord8
          case utf8 char1 char2 char3 char4 w0 of
            UTF8_1 c  -> (:) c <$> go

            Part_2 f0 -> do
              mayw1 <- peekWord8
              case mayw1 of
                Just w1 ->
                  case f0 w1 of
                    UTF8_2 c  -> do _ <- anyWord8
                                    (:) c <$> go
                    Error_2 e -> recover e

                Nothing -> recover $ Incomplete22 w0 0

            Part_3_1 f0 -> do
              mayw1 <- peekWord8
              case mayw1 of
                Just w1 ->
                  case f0 w1 of
                    Part_3_2 f1 -> do
                      _ <- anyWord8
                      mayw2 <- peekWord8
                      case mayw2 of
                        Just w2 ->
                          case f1 w2 of
                            UTF8_3 c    -> do _ <- anyWord8
                                              (:) c <$> go

                            Error_3_2 e -> recover e

                        Nothing -> recover $ Incomplete33 w0 w1 0

                    Error_3_1 e -> recover e

                Nothing -> recover $ Incomplete23 w0 0

            Part_4_1 f0 -> do
              mayw1 <- peekWord8
              case mayw1 of
                Just w1 ->
                  case f0 w1 of
                    Part_4_2 f1 -> do
                      _ <- anyWord8
                      mayw2 <- peekWord8
                      case mayw2 of
                        Just w2 ->
                          case f1 w2 of
                            Part_4_3 f2    -> do
                              _ <- anyWord8
                              mayw3 <- peekWord8
                              case mayw3 of
                                Just w3 ->
                                  case f2 w3 of
                                    UTF8_4 c    -> do _ <- anyWord8
                                                      (:) c <$> go

                                    Error_4_3 e -> recover e

                                Nothing -> recover $ Incomplete4 w0 w1 w2 0

                            Error_4_2 e -> recover e

                        Nothing -> recover $ Incomplete34 w0 w1 0

                    Error_4_1 e -> recover e

                Nothing -> recover $ Incomplete24 w0 0

            Error_1 e -> recover e

        Nothing -> pure []
