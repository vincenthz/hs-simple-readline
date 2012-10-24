-- |
-- Module      : System.Console.SimpleReadline.Terminal
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- terminal related manipulation
--
module System.Console.SimpleReadline.Terminal
    where

import System.IO
import Control.Monad
import Control.Monad.State
import qualified Control.Exception as E
import System.Console.SimpleReadline.Types

termSet (bufIn, bufOut, echoOut) =
    hSetBuffering stdin bufIn   >>
    hSetBuffering stdout bufOut >>
    hSetEcho stdout echoOut

move :: (Monad m, MonadIO m) => String -> Int -> ReadlineT m ()
move mvStr n = liftIO $ putStr $ concat $ replicate n mvStr

moveLeft :: (Monad m, MonadIO m) => Int -> ReadlineT m ()
moveLeft = move "\ESC[D"

moveRight :: (Monad m, MonadIO m) => Int -> ReadlineT m ()
moveRight = move "\ESC[C"

moveHome :: (Monad m, MonadIO m) => ReadlineT m ()
moveHome = move "\r" 1

termGet = liftM3 (,,) (hGetBuffering stdin) (hGetBuffering stdout) (hGetEcho stdout)

termInit = termGet >>= \old -> termSet (NoBuffering, NoBuffering, False) >> return old

withTerm :: IO a -> IO a
withTerm f = termInit >>= \old -> f `E.finally` termSet old

getNext :: ReadlineT IO Key
getNext = do gets rlKeyHandlers >>= loop []
    where loop st hs = do c <- liftIO (hGetChar stdin)
                          case findInTree c hs of
                               Nothing         -> {-liftIO (putStrLn ("other: " ++ show (reverse (c:st)))) >> -} return (KeyOther c)
                               Just (Leaf key) -> return key -- liftIO $ putStrLn (show key)
                               Just n@(Node _) -> loop (c:st) n
