module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import System.Exit (exitFailure, exitSuccess)

import ParLatte
import ErrM

import CommonDeclarations
import CodeGenerator
import StaticAnalyser


fromMaybe :: Maybe a -> a
fromMaybe (Just x) = x
fromMaybe Nothing = error "parse tree transform error"

parse :: Maybe String -> IO CursorProgram
parse file = do
  contents <- maybe getContents readFile file
  case pProgram $ myLexer contents of
    Bad s -> do
        printError $ SyntaxError s
        exitFailure
    Ok tree -> return $ fmap fromMaybe tree


main :: IO ()
main = do
  args <- getArgs
  (_, outputFile, _) <- parseArguments args ".ll"
  let
    file = case args of
      (s:_) -> Just s
      [] -> Nothing
  tree <- parse file
  staticAnalysisResult <- runStaticAnalysis tree
  case staticAnalysisResult of
    Right result -> do
      printErrorString "OK"
      runGenerateCode result outputFile
    Left e -> printError e >> exitFailure
  exitSuccess
