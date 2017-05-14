{-# LANGUAGE DataKinds,
             KindSignatures #-}

import           Data.Monoid
import           Data.Maybe (isJust, fromJust)
import qualified Data.Text.IO as IO

import Language.Hakaru.Command hiding (Term)
import Language.Hakaru.Types.DataKind
import Language.Hakaru.Types.Sing
import Language.Hakaru.Syntax.ABT
import Language.Hakaru.Syntax.AST
import Language.Hakaru.Syntax.TypeCheck

import System.IO
import System.Process
import System.Exit
import System.Directory
import System.FilePath.Posix
import System.Environment

type Test = FilePath

main :: IO ()
main = do
  args <- getArgs
  let (hkc,cc) = case args of
                  (hkc:cc:[]) -> (hkc,cc)
                  _           -> error "Usage: hkc-tests <HKC> <CC>"

  tests      <- getTests
  createDirectoryIfMissing False "build"

  putStrLn $ stars <> "TESTING HKC" <> stars
  tests' <- mapM (hakaruToC hkc) tests

  putStrLn $ stars <> "TESTING CC" <> stars
  tests'' <- mapM (cToBinary cc . fromJust) . filter isJust $ tests'

  -- putStrLn $ stars <> "TESTING OUTPUT" <> stars
  -- tests''' <- mapM (binaryToOutput . fromJust) . filter isJust $ tests''

  reportStats "HKC Tests" tests'
  reportStats "CC Tests" tests''
  -- reportStats "Output Tests" tests'''
  putStrLn "Fin."

getTests :: IO [FilePath]
getTests = filter ((== ".hk") . takeExtension) <$> listDirectory "tests/hakaru"

getHkTypedTerm :: FilePath -> IO (Maybe (TypedAST (TrivialABT Term)))
getHkTypedTerm fp = do
  prog  <- IO.readFile fp
  case parseAndInfer prog of
    Left err               -> IO.hPutStrLn stderr err >> return Nothing
    Right term -> return (Just term)

getStats :: [Maybe a] -> (Int,Int)
getStats xs =
  foldr (\x (s,t) -> case x of
                      Just _  -> (succ s,succ t)
                      Nothing -> (s, succ t)
        ) (0,0) xs

reportStats :: String -> [Maybe a] -> IO ()
reportStats test xs =
   putStrLn $ stars <> "\n" <> test <> ": passed "
           <> (show success) <> " of " <> (show total)
           <> "\n" <> stars
   where (success,total) = getStats xs

stars :: String
stars = "\n" <> replicate 80 '*' <> "\n"

--------------------------------------------------------------------------------
--                                 TESTS                                      --
--------------------------------------------------------------------------------

{-
Take in a Hakaru source file and produce a C file. Encountered errors here are
incomplete implementations of Hakaru programs in HKC
-}
hakaruToC :: String -> FilePath -> IO (Maybe FilePath)
hakaruToC hkc fp =
  let fp' = "build" </> "sea" </> (takeBaseName fp) <.> "c"
      process = proc hkc ["-g"
                         ,"tests" </> "hakaru" </> fp
                         ,"-o"
                         ,fp']
  in
    do createDirectoryIfMissing False ("build" </> "sea")
       putStrLn $ "hkc: ( " <> fp <> " , " <> fp' <> " )"
       (_,_,Just errH,pH) <- createProcess process { std_err = CreatePipe }
       exitcode <- waitForProcess pH
       case exitcode of
         ExitSuccess -> return (Just fp')
         _ -> do putStrLn "FAILED"
                 err <- hGetContents errH
                 appendFile "build/hkc.log" (stars <> fp <>":\n\n" <> err)
                 return Nothing

{-
Take the C code produced by HKC and compile to an executable using some C compiler.
Encountered here could be missing libraries and bad code generation.  
-}
cToBinary :: String -> FilePath -> IO (Maybe FilePath)
cToBinary cc fp =
  let fp' = "build" </> "bin" </> (takeBaseName fp)
      process = proc cc ["-O3","-march=native","-lm","-lgc","-std=c99","-pedantic"
                        ,fp,"-o",fp']
  in
    do createDirectoryIfMissing False ("build" </> "bin")
       putStrLn $ "cc: ( " <> fp <> " , " <> fp' <> " )"
       (_,_,Just errH,pH) <- createProcess process { std_err = CreatePipe }
       exitcode <- waitForProcess pH
       case exitcode of
         ExitSuccess -> return (Just fp')
         _ -> do putStrLn "FAILED"
                 err <- hGetContents errH
                 appendFile "build/cc.log" (stars <> fp <>":\n\n" <> err)
                 return Nothing

{-
This is a more complicated then the other two tests. It needs to see if there is
an input file in the `tests/input` directory, parse the input file, and feed its
contents to the executable.

If the program is a measure it will draw 10 samples with a `-n10` flag to the
executable.

Possible errors here
-}
binaryToOutput :: FilePath -> IO (Maybe FilePath)
binaryToOutput fp =
  let fp' = "build" </> "output" </> fp </> "out"
      process = proc fp [">",fp']
  in
    do createDirectoryIfMissing False ("build" </> "output")
       putStrLn $ "output: ( " <> fp <> " , " <> fp' <> " )"
       (_,_,Just errH,pH) <- createProcess process { std_err = CreatePipe }
       exitcode <- waitForProcess pH
       case exitcode of
         ExitSuccess -> return (Just fp')
         _ -> do putStrLn "FAILED"
                 err <- hGetContents errH
                 appendFile "build/output.log" (stars <> fp <>":\n\n" <> err)
                 return Nothing

{-
Takes the output of a run HKC generated program and tests it against the output
in `tests/output`. Problems here could be that there is no standardized output
or floating point errors.
-}  
checkOutput :: FilePath -> IO (Maybe Bool)
checkOutput = undefined
