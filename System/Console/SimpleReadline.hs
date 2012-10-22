-- |
-- Module      : System.Console.SimpleReadline
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- simple readline implementation
--
module System.Console.SimpleReadline
    ( readline
    , readlineStateDefault
    , ReadlineState(..)
    -- * handlers
    , PrefixTree(..)
    , defaultKeyHandlers
    , defaultFnHandlers
    , otherHandler
    -- * handlers operations
    , setQuit
    , setPrompt
    , moveLeft
    , moveRight
    , modifyCurrentLine
    ) where

import System.Console.SimpleReadline.Types
import System.Console.SimpleReadline.Terminal
import System.Console.SimpleReadline.Zipper

import Control.Monad.Trans
import Control.Monad.State

defaultKeyHandlers =
    Node [ ('\ESC', Node [ ('[', Node [ ('D', fn LeftArrow)
                                      , ('C', fn RightArrow)
                                      , ('A', fn UpArrow)
                                      , ('B', fn DownArrow)
                                      , ('1', Node [ (';', Node [ ('5', Node [ ('A', fn CtrlUpArrow)
                                                                             , ('B', fn CtrlDownArrow)
                                                                             , ('C', fn CtrlRightArrow)
                                                                             , ('D', fn CtrlLeftArrow)
                                                                             ])
                                                                ])
                                                   ])
                                      , ('2', Node [ ('~', fn Insert) ])
                                      , ('3', Node [ ('~', fn Del) ])
                                      , ('5', Node [ ('~', fn PageUp) ])
                                      , ('6', Node [ ('~', fn PageDown) ])
                                      ])
                         , ('O', Node [ ('H', fn Home)
                                      , ('F', fn End)
                                      ])
                         ]
           )
         , ('\DEL', fn Backspace)
         , ('\GS', fn CtrlBracketRight)  -- Ctrl+]
         , ('\SOH', fn Home) -- Ctrl+a
         , ('\SO', fn NextLine) -- Ctrl+n
         , ('\DLE', fn PrevLine) -- Ctrl+p
         , ('\ENQ', fn End)  -- Ctrl+e
         , ('\DC2', fn SearchBackwardHistory) -- Ctrl+r
         , ('\STX', fn Backward) -- Ctrl+b
         , ('\ACK', fn Forward) -- Ctrl+f
         , ('\EOT', fn ForwardDel) -- Ctrl+d (could be EOF if line is empty)
         , ('\ETB', fn DelWord) -- Ctrl+w
         , ('\NAK', fn DelAll) -- Ctrl+u
         , ('\a', fn Bell) -- Ctrl+g
         , ('\b', fn DelPrev) -- Ctrl+g
         , ('\t', fn Tab)
         , ('\f', fn ClearScreen) -- Ctrl+l
         , ('\n', fn Enter)
         , ('\v', fn DelTillEOL) -- Ctrl+k
         ]
    where fn = Leaf . KeyFn

defaultFnHandlers =
    [ (LeftArrow, handlerMovePrev)
    , (RightArrow, handlerMoveNext)
    , (Home, handlerMoveHome)
    , (End, handlerMoveEnd)
    , (Backspace, handlerDelBackward)
    , (DelTillEOL, handlerDelTillEOL)
    ]

modifyCurrentLine :: (Zipper Char -> Zipper Char) -> Readline ()
modifyCurrentLine f = modify (\st -> st { rlCurrentLine = f $ rlCurrentLine st })


whenHasNext f = gets rlCurrentLine >>= \z -> when (zipHasNext z) f
whenHasPrev f = gets rlCurrentLine >>= \z -> when (zipHasPrev z) f

redisp post =
    moveHome >>
    gets rlPrompt >>= liftIO . putStr >>
    gets rlCurrentLine >>= \z -> liftIO (putStr (zipToList z)) >> liftIO (putStr post)

displayToEnd pre post = gets rlCurrentLine >>= disp
	where disp (Zipper _ _ nlen n) = liftIO (putStr (pre ++ n ++ post)) >> moveLeft (nlen + length post)
otherHandler c = modifyCurrentLine (zipInsert [c]) >> displayToEnd [c] ""

readlineStateDefault = ReadlineState (zipInit []) (zipInit []) "" False defaultKeyHandlers defaultFnHandlers otherHandler

-- simple moves
handlerMovePrev = whenHasPrev (moveLeft 1 >> modifyCurrentLine zipPrev)
handlerMoveNext = whenHasNext (moveRight 1 >> modifyCurrentLine zipNext)

-- other moves
handlerMoveHome = gets rlCurrentLine >>= moveLeft . zipLengthPrev >> modifyCurrentLine zipAtHome
handlerMoveEnd = gets rlCurrentLine >>= moveRight . zipLengthNext >> modifyCurrentLine zipAtEnd

-- deletions
handlerDelBackward = whenHasPrev (moveLeft 1 >> modifyCurrentLine (zipDelPrev 1) >> displayToEnd "" " ")
handlerDelTillEOL  = gets rlCurrentLine >>= \z -> displayToEnd "" (replicate (zipLengthNext z) ' ') >> modifyCurrentLine zipDelToEnd

-- accept
handlerEnter f =
    gets (zipToList . rlCurrentLine) >>= \l -> liftIO (f l) >>
    modifyCurrentLine (const (zipInit "")) >>
    gets rlPrompt >>= liftIO . putStr

readline st f = withTerm $ runReadline st (redisp "" >> loop)
    where loop = do
            n <- getNext
            case n of
                KeyFn Enter -> handlerEnter f
                KeyFn keyfn -> do fnHandlers <- gets rlFnHandlers
                                  case lookup keyfn fnHandlers of
                                      Nothing -> return ()
                                      Just h  -> h
                KeyOther c  -> do oh <- gets rlOtherHandler
                                  oh c
            doQuit <- gets rlQuit
            if doQuit
                then return ()
                else loop

setPrompt :: String -> Readline ()
setPrompt s = modify (\st -> st { rlPrompt = s })

setQuit :: Readline ()
setQuit = modify (\st -> st { rlQuit = True })
