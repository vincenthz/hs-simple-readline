-- |
-- Module      : System.Console.SimpleReadline.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- types for readline
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Console.SimpleReadline.Types
    ( KeyFn(..)
    , Key(..)
    , PrefixTree(..)
    , findInTree
    , Readline
    , ReadlineT
    , ReadlineState(..)
    , runReadline
    )
    where

import Control.Applicative
import Control.Monad.State
import System.Console.SimpleReadline.Zipper

data KeyFn = LeftArrow
           | RightArrow
           | UpArrow
           | DownArrow
           | CtrlUpArrow
           | CtrlDownArrow
           | CtrlLeftArrow
           | CtrlRightArrow
           | CtrlBracketRight
           | Enter
           | ClearScreen
           | Tab
           | Backward
           | Forward
           | Backspace
           | PrevLine
           | NextLine
           | Home
           | End
           | Insert
           | SearchBackwardHistory
           | Del
           | DelPrev
           | DelTillEOL
           | DelWord
           | DelAll
           | PageUp
           | PageDown
           | ForwardDel
           | Bell
           deriving (Show,Eq)

data Key = KeyFn    KeyFn   -- recognized function
         | KeyOther Char    -- unrecognized input
         deriving (Show,Eq)

data PrefixTree key val = Node [(key, PrefixTree key val)]
                        | Leaf val

findInTree x (Leaf _) = Nothing
findInTree x (Node l) = lookup x l

data ReadlineState = ReadlineState { rlHistory      :: Zipper String
                                   , rlCurrentLine  :: Zipper Char
                                   , rlPrompt       :: String
                                   , rlQuit         :: Bool
                                   , rlKeyHandlers  :: PrefixTree Char Key
                                   , rlFnHandlers   :: [ (KeyFn, Readline ()) ]
                                   , rlOtherHandler :: (Char -> Readline ())
                                   }

newtype ReadlineT m a = ReadlineT { unReadlineT :: StateT ReadlineState m a }
    deriving (Monad, MonadIO, MonadState ReadlineState, Functor, Applicative)

type Readline = ReadlineT IO

runReadline :: Monad m => ReadlineState -> ReadlineT m a -> m (a, ReadlineState)
runReadline st f = runStateT (unReadlineT f) st
