module Main where

import Lib
import Task3
import System.Environment
import System.Exit
import System.IO (hPutStrLn, stderr)

main :: IO()
main = do
    args <- getArgs
    msg <- getLine
    let 
        (myStdOut, myErrOut, myExitCode) = if (msg == "*") then getOutput "*" else getOutput msg in
        case myExitCode of
            100 -> do exitWith $ ExitFailure myExitCode
            101 -> do exitWith $ ExitFailure myExitCode
            _ -> do
                putStrLn myStdOut
                hPutStrLn stderr myErrOut
                case myExitCode of
                    0 -> exitWith ExitSuccess
                    _ -> exitWith $ ExitFailure myExitCode
