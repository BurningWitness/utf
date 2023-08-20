module Parser.Handler where

import           Codec.Encoding.UTF8



data Action = Die String
            | Replace Char
            | Ignore

newtype NaiveHandler = NaiveHandler (Error -> Action)
