------------------------------------------------------------------------------------------
-- CleverCSS in Haskell, (c) 2007, 2008 Georg Brandl. Licensed under the BSD license.
--
-- CCMain module: command-line interface.
------------------------------------------------------------------------------------------

module Main (main) where

import Control.Monad (when)
import Data.List (findIndices)
import System.Environment
import System.Console.GetOpt
import System.IO

import Text.CSS.CleverCSS

-- read an option definition from a command line option
readDef arg = case span (/= '=') arg of
                (_,    "")  -> error "invalid variable definition, must be name=value"
                (_,    "=") -> error "empty variable value"
                (name, val) -> (name, tail val)

optdescr = [Option "D" [] (ReqArg readDef "name=value") "define a variable",
            Option "h" ["help"] (NoArg ("help","")) "print this help message"]

help errs = do
  pname <- getProgName
  when ((not.null) errs) $ putStrLn $ concat errs
  putStrLn (usageInfo ("Usage: " ++ pname ++ " [opts] [file ...]\n\
          \\n\
          \With file names, read these and convert them to CSS files.\n\
          \The output file name is determined by stripping the input\n\
          \file names' extensions and adding `.css'. Existing files are\n\
          \overwritten.\n\
          \Without file names, read from stdin and write to stdout.\n\n\
          \Options:\n") optdescr)

main :: IO ()
main = do
  rawargs <- getArgs
  let (defs, args, errs) = getOpt Permute optdescr rawargs
  if (not.null) errs || ("help","") `elem` defs then help errs else
    case length args of
      0 -> process getContents "stdin" stdout defs >> return ()
      _ -> mapM_ (processFile defs) args
  where
    processFile defs filename = do
         let outfilename = case findIndices (=='.') filename of
                             [] -> filename ++ ".css"
                             is -> take (last is) filename ++ ".css"
         if outfilename == filename then
           fail $ "Input and output filenames are equal: " ++ filename
           else do
             outfile <- openFile outfilename WriteMode
             result <- process (readFile filename) filename outfile defs
             hClose outfile
             when result $ putStr $
                    "Converted file " ++ filename ++ " to " ++ outfilename ++ ".\n"
    process inaction filename outfile defs = do
         input <- inaction
         result <- cleverCSSConvert filename input defs
         case result of
           Left err  -> do
             hPutStr stderr $ "While processing " ++ filename ++ ":\n" ++ err ++ "\n"
             return False
           Right out -> do
             hPutStr outfile out
             return True
