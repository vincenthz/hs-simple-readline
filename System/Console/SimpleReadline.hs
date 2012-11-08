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
    , Readline
    , ReadlineT(..)
    -- * handlers
    , PrefixTree(..)
    , defaultKeyHandlers
    , defaultFnHandlers
    , otherHandler
    , EnterBehavior(..)
    -- * handlers operations
    , setQuit
    , setPrompt
    , moveLeft
    , moveRight
    , modifyCurrentLine
    -- * zipper operations
    , module System.Console.SimpleReadline.Zipper
    ) where

import System.Console.SimpleReadline.Types
import System.Console.SimpleReadline.Terminal
import System.Console.SimpleReadline.Zipper

import Control.Applicative
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
                                      , ('2', Node [ ('~', fn Insert) ]) -- Insert key
                                      , ('3', Node [ ('~', fn Del) ]) -- Delete key
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
         , ('\EOT', fn DelForward) -- Ctrl+d (could be EOF if line is empty)
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
    [ (LeftArrow , handlerMovePrev)
    , (RightArrow, handlerMoveNext)
    , (UpArrow   , handlerHistoryUp)
    , (DownArrow , handlerHistoryDown)
    , (Home      , handlerMoveHome)
    , (End       , handlerMoveEnd)
    , (Backspace , handlerDelBackward)
    , (DelTillEOL, handlerDelTillEOL)
    , (DelAll    , handlerDelAll)
    , (DelForward, handlerDelForward)
    ]

data EnterBehavior = Quit | Retry | Ok
type Handler = Readline ()

modifyCurrentLine :: (Zipper Char -> Zipper Char) -> Readline ()
modifyCurrentLine f = modify (\st -> st { rlCurrentLine = f $ rlCurrentLine st })

modifyHistory :: (Zipper String -> Zipper String) -> Readline ()
modifyHistory f = modify (\st -> st { rlHistory = f $ rlHistory st })

whenHasNext f = gets rlCurrentLine >>= \z -> when (zipHasNext z) f
whenHasPrev f = gets rlCurrentLine >>= \z -> when (zipHasPrev z) f

redisp :: (Monad m, MonadIO m) => String -> ReadlineT m ()
redisp post =
    moveHome >>
    gets rlPrompt >>= liftIO . putStr >>
    gets rlCurrentLine >>= \z -> liftIO (putStr (zipToList z)) >> liftIO (putStr post)

displayToEnd pre post = gets rlCurrentLine >>= disp
	where disp (Zipper _ _ nlen n) = liftIO (putStr (pre ++ n ++ post)) >> moveLeft (nlen + length post)
otherHandler c = modifyCurrentLine (zipInsert [c]) >> displayToEnd [c] ""

readlineStateDefault = ReadlineState (zipInit []) (zipInit []) "" False defaultKeyHandlers defaultFnHandlers otherHandler


-- simple moves
handlerMovePrev, handlerMoveNext :: Handler
handlerMovePrev = whenHasPrev (moveLeft 1 >> modifyCurrentLine zipPrev)
handlerMoveNext = whenHasNext (moveRight 1 >> modifyCurrentLine zipNext)

-- other moves
handlerMoveHome, handlerMoveEnd :: Handler
handlerMoveHome = gets rlCurrentLine >>= moveLeft . zipLengthPrev >> modifyCurrentLine zipAtHome
handlerMoveEnd = gets rlCurrentLine >>= moveRight . zipLengthNext >> modifyCurrentLine zipAtEnd

-- history
handlerHistoryUp, handlerHistoryDown :: Handler
handlerHistoryUp = gets (zipToList . rlCurrentLine) >>= \cl -> modifyHistory (zipPrev . zipInsert [cl])
    >> insertHistory
    
handlerHistoryDown = gets (zipToList . rlCurrentLine) >>= \cl -> modifyHistory (zipNext . zipInsert [cl])
    >> insertHistory

insertHistory = return () -- get rlCurrentLine >>= \z -> gets (rlHistory) >>= \_ -> redisp ""

-- deletions
handlerDelBackward, handlerDelTillEOL, handlerDelAll, handlerDelForward :: Handler
handlerDelBackward = whenHasPrev (moveLeft 1 >> modifyCurrentLine (zipDelPrev 1) >> displayToEnd "" " ")
handlerDelTillEOL  = gets (zipLengthNext . rlCurrentLine) >>= \len ->
    modifyCurrentLine zipDelToEnd >> displayToEnd "" (replicate len ' ')

handlerDelAll = gets (zipLength . rlCurrentLine) >>= \len ->
    modifyCurrentLine (const (zipInit "")) >> redisp (replicate len ' ') >> moveLeft len

handlerDelForward = gets rlCurrentLine >>= \z ->
    if zipLength z == 0
        then setQuit >> liftIO (putStrLn "")
        else whenHasPrev (modifyCurrentLine (zipDelNext 1) >> displayToEnd "" " ")

-- accept
handlerEnter :: (String -> Readline EnterBehavior) -> Handler
handlerEnter f =
    gets (zipToList . rlCurrentLine) >>= \l -> (liftIO (putStrLn "") >> f l) >>= \eb ->
    case eb of
        Quit  -> setQuit
        Retry -> redisp ""
        Ok    -> modifyCurrentLine (const (zipInit "")) >> gets rlPrompt >>= liftIO . putStr

readline :: ReadlineState IO -> (String -> Readline EnterBehavior) -> IO (ReadlineState IO)
readline st f = withTerm (snd <$> runReadline st (redisp "" >> loop))
    where loop :: ReadlineT IO ()
          loop = do
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

setPrompt :: (Monad m, MonadIO m) => String -> ReadlineT m ()
setPrompt s = modify (\st -> st { rlPrompt = s })

setQuit :: (Monad m, MonadIO m) => ReadlineT m ()
setQuit = modify (\st -> st { rlQuit = True })
