{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils (
    IndentM,
    IndentData,
    appChar,
    indented, indentedNewLine,
    evalIndent,
    withIndent
    ) where

import Control.Applicative
import Control.Monad.RWS
import Data.String (IsString(..))

type IndentData = (Endo String, Any, Sum Int)
newtype IndentM a = MkRWS { runIndent :: RWS Int IndentData () a }
    deriving (Functor, Applicative, Monad, MonadReader Int, MonadWriter IndentData)

instance Semigroup a => Semigroup (IndentM a) where
    (MkRWS ma) <> (MkRWS mb) = MkRWS $ liftA2 (<>) ma mb

instance IsString (IndentM ()) where
    fromString str = MkRWS $ tell (Endo (str++), Any $ elem '\n' str, Sum $ length str)

appChar :: Char -> IndentM ()
appChar c = tell (Endo (c:), Any $ c == '\n', Sum 1)

indented :: IndentM ()
indented = ask >>= \ind -> fromString $ replicate ind ' '

indentedNewLine :: IndentM ()
indentedNewLine = "\n" <> indented

withIndent :: IndentM a -> IndentM a
withIndent (MkRWS m) = MkRWS $ local (+1) m

evalIndent :: IndentM a -> (a, IndentData)
evalIndent m = evalRWS (runIndent m) 0 ()
