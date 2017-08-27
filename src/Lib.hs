module Lib
    ( main
    ) where

import System.IO
import System.IO.Error

import Base
import Parser
import Language
import Running
import Types
import Builtins

main :: IO ()
main = repl Nothing

repl :: Maybe String -> IO ()
repl last = do
    putStr "\n> "
    hFlush stdout
    line' <- getLineEOF
    putStrLn ""
    case line' of
        Just line -> do
            case langParse line of
                Right a -> do
                    case typeCheck a T_Any defaultTypeContext of
                        Right (t, ctx) -> do
                            reduced <- run a False
                            putStrLn $ "< " ++ prettyShow reduced ++ " :: " ++ show t
                            repl $ Just line
                        Left err -> do
                            putStrLn $ "<! Error: " ++ show err
                            repl $ Just line
                Left err -> do
                    if line == "?"
                        then case last of
                            Just x -> debug x >> repl last
                            Nothing -> do
                                putStrLn "Nothing evaluated last time"
                                repl last
                        else do
                            putStrLn err
                            repl Nothing
        Nothing -> return ()

debug :: String -> IO ()
debug code =
    case langParse code of
        Right ast -> do
            putStrLn $ "<\n" ++ unlines (dindent ["Code before"] [diagram ast])
            putStrLn $ "Type before: " ++ show (typeCheck ast T_Any defaultTypeContext)
            reduced <- run ast True
            putStrLn $ "<\n" ++ unlines (dindent ["Code after"] [diagram reduced])
            putStrLn $ "Type after (shouldn't have changed): " ++ show (typeCheck reduced T_Any defaultTypeContext)
        Left err ->
            putStrLn $ "Code didn't compile: " ++ show err


getLineEOF = do
    x <- fmap Just getLine `catchIOError` (\e -> if isEOFError e then return Nothing else ioError e)
    return x

run :: Ast -> Bool -> IO Ast
run a step = do
    let reduced = reduce a defaultContext
    if reduced == a
        then do
            return reduced
        else do
            if step
                then putStrLn $ ">\n" ++ unlines (diagram reduced)
                else return ()
            run reduced step
