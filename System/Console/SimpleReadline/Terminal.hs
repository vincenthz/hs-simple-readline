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

move :: String -> Int -> Readline ()
move mvStr n = liftIO $ putStr $ concat $ replicate n mvStr

moveLeft :: Int -> Readline ()
moveLeft = move "\ESC[D"

moveRight :: Int -> Readline ()
moveRight = move "\ESC[C"

termGet = liftM3 (,,) (hGetBuffering stdin) (hGetBuffering stdout) (hGetEcho stdout)

termInit = termGet >>= \old -> termSet (NoBuffering, NoBuffering, False) >> return old

withTerm f = termInit >>= \old -> (f `E.finally` termSet old)

getNext :: Readline Key
getNext = do gets rlKeyHandlers >>= loop []
    where loop st hs = do c <- liftIO (hGetChar stdin)
                          case findInTree c hs of
                               Nothing         -> return $ KeyOther c -- liftIO $ putStrLn ("other: " ++ show (reverse (c:st)))
                               Just (Leaf key) -> return key -- liftIO $ putStrLn (show key)
                               Just n@(Node _) -> loop (c:st) n
