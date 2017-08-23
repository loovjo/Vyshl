module Lib
    ( main
    ) where

import System.IO
import System.IO.Error

import Base
import Parser
import Language
import Running

main :: IO ()
main = do
    putStr "> "
    hFlush stdout
    line' <- getLineEOF
    case line' of
        Just line -> do
            case langParse line of
                Right a -> do
                    putStrLn $ "< \n" ++ (unlines $ map ("  " ++) $ diagram a)
                    reduced <- run a
                    putStrLn $ "< \n" ++ (unlines $ map ("  " ++) $ diagram reduced)
                    putStrLn $ "< \n" ++ prettyShow reduced
                Left err -> putStrLn $ "Error: " ++ err
            main
        Nothing -> return ()


getLineEOF = do
    x <- fmap Just getLine `catchIOError` (\e -> if isEOFError e then return Nothing else ioError e)
    return x

run :: Ast -> IO Ast
run a = do
    let reduced = reduce a Empty
    if reduced == a
        then do
            putStrLn "Done!"
            return reduced
        else do 
            putStrLn $ "<< " ++ show reduced
            run reduced
