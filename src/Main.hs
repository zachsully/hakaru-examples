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

newtype Test = Test String

instance Show Test where
  show (Test name) = "Test: " <> show name

testName :: Test -> String
testName (Test name) = name

testFP,inputFP,correctOutputFP,outputFP,seaFP,binFP,hsFP,hsBinFP
  :: Test -> FilePath
testFP          (Test name) = "tests" </> "hakaru"  </> name <.> "hk"
inputFP         (Test name) = "tests" </> "input"   </> name <.> "in"
correctOutputFP (Test name) = "tests" </> "output"  </> name <.> "out"
outputFP        (Test name) = "build" </> "output"  </> name <.> "out"
seaFP           (Test name) = "build" </> "sea"     </> name <.> "c"
binFP           (Test name) = "build" </> "bin"     </> name <.> "c" <.> "bin"
hsFP            (Test name) = "build" </> "haskell" </> name <.> "hs"
hsBinFP         (Test name) = "build" </> "bin"     </> name <.> "hs" <.> "bin"


main :: IO ()
main = do
  args <- getArgs
  let (hkc,cc) = case args of
                  (hkc:cc:[]) -> (hkc,cc)
                  _           -> error "Usage: hkc-tests <HKC> <CC>"

  tests      <- getTests
  createDirectoryIfMissing False "build"

  putStrLn $ stars <> "COMPILE" <> stars
  testsHs <- mapM (hakaruToHs "compile") tests

  putStrLn $ stars <> "GHC" <> stars
  testsHs' <- mapM (hsToBinary "ghc") testsHs

  putStrLn $ stars <> "HKC" <> stars
  tests' <- mapM (hakaruToC hkc) tests

  putStrLn $ stars <> "CC" <> stars
  tests'' <- mapM (cToBinary cc . fst) . filter snd $ tests'

  -- putStrLn $ stars <> "TESTING OUTPUT" <> stars
  -- tests''' <- mapM (binaryToOutput . fromJust) . filter isJust $ tests''

  reportStats "Compile Tests" testsHs
  reportStats "Haskell Tests" testsHs'
  reportStats "HKC Tests" tests'
  reportStats "CC Tests" tests''
  -- reportStats "Output Tests" tests'''
  putStrLn "Fin."

getTests :: IO [Test]
getTests = do
   files <- listDirectory ("tests" </> "hakaru")
   return . fmap (Test . takeBaseName) . filter ((== ".hk") . takeExtension) $ files

getHkTypedTerm :: FilePath -> IO (Maybe (TypedAST (TrivialABT Term)))
getHkTypedTerm fp = do
  prog  <- IO.readFile fp
  case parseAndInfer prog of
    Left err               -> IO.hPutStrLn stderr err >> return Nothing
    Right term -> return (Just term)

getStats :: [(Test,Bool)] -> (Int,Int)
getStats xs =
  foldr (\x (s,t) -> case snd x of
                      True  -> (succ s,succ t)
                      False -> (s, succ t)
        ) (0,0) xs

reportStats :: String -> [(Test,Bool)] -> IO ()
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
hakaruToC :: String -> Test -> IO (Test,Bool)
hakaruToC hkc test =
  let process = proc hkc ["-g",testFP test,"-o",seaFP test]
  in
    do createDirectoryIfMissing False ("build" </> "sea")
       putStrLn $ "hkc: ( " <> testFP test <> " , " <> seaFP test <> " )"
       (_,_,Just errH,pH) <- createProcess process { std_err = CreatePipe }
       exitcode <- waitForProcess pH
       case exitcode of
         ExitSuccess -> return (test,True)
         _ -> do putStrLn "FAILED"
                 err <- hGetContents errH
                 appendFile "build/hkc.log" (stars <> testFP test <>":\n\n" <> err)
                 return (test,False)

{-
Take in a Hakaru source file and produce a Haskell file
-}
hakaruToHs :: String -> Test -> IO (Test,Bool)
hakaruToHs compile test =
  let process = proc compile [testFP test,"-o", hsFP test]
  in
    do createDirectoryIfMissing False ("build" </> "haskell")
       putStrLn $ "compile: ( " <> testFP test <> " , " <> hsFP test <> " )"
       (_,_,Just errH,pH) <- createProcess process { std_err = CreatePipe }
       exitcode <- waitForProcess pH
       case exitcode of
         ExitSuccess -> return (test,True)
         _ -> do putStrLn "FAILED"
                 err <- hGetContents errH
                 appendFile "build/hkhs.log" (stars <> testFP test <>":\n\n" <> err)
                 return (test,False)

hsToBinary :: String -> Test -> IO (Test,Bool)
hsToBinary ghc test =
  let process = proc ghc ["-O2",hsFP test,"-o", hsBinFP test]
  in
    do createDirectoryIfMissing False ("build" </> "haskell")
       putStrLn $ "compile: ( " <> testFP test <> " , " <> hsFP test <> " )"
       (_,_,Just errH,pH) <- createProcess process { std_err = CreatePipe }
       exitcode <- waitForProcess pH
       case exitcode of
         ExitSuccess -> return (test,True)
         _ -> do putStrLn "FAILED"
                 err <- hGetContents errH
                 appendFile "build/hs.log" (stars <> testFP test <>":\n\n" <> err)
                 return (test,False)

{-
Take the C code produced by HKC and compile to an executable using some C compiler.
Encountered here could be missing libraries and bad code generation.
-}
cToBinary :: String -> Test -> IO (Test,Bool)
cToBinary cc test =
  let process = proc cc ["-O3","-march=native","-lm","-lgc","-std=c99","-pedantic"
                        ,seaFP test,"-o",binFP test]
  in
    do createDirectoryIfMissing False ("build" </> "bin")
       putStrLn $ "cc: ( " <> seaFP test <> " , " <> binFP test <> " )"
       (_,_,Just errH,pH) <- createProcess process { std_err = CreatePipe }
       exitcode <- waitForProcess pH
       case exitcode of
         ExitSuccess -> return (test,True)
         _ -> do putStrLn "FAILED"
                 err <- hGetContents errH
                 appendFile "build/cc.log" (stars <> seaFP test <>":\n\n" <> err)
                 return (test,False)

{-
This is a more complicated then the other two tests. It needs to see if there is
an input file in the `tests/input` directory, parse the input file, and feed its
contents to the executable.

If the program is a measure it will draw 10 samples with a `-n10` flag to the
executable.

Possible errors here
-}
binaryToOutput :: Test -> IO (Test,Bool)
binaryToOutput test =
  let process = proc (binFP test) [">",outputFP test]
  in
    do createDirectoryIfMissing False ("build" </> "output")
       putStrLn $ "output: ( " <> binFP test <> " , " <> outputFP test <> " )"
       (_,_,Just errH,pH) <- createProcess process { std_err = CreatePipe }
       exitcode <- waitForProcess pH
       case exitcode of
         ExitSuccess -> return (test,True)
         _ -> do putStrLn "FAILED"
                 err <- hGetContents errH
                 appendFile "build/output.log" (stars <> binFP test <>":\n\n" <> err)
                 return (test,False)

{-
Takes the output of a run HKC generated program and tests it against the output
in `tests/output`. Problems here could be that there is no standardized output
or floating point errors.
-}
checkOutput :: Test -> IO (Maybe Bool)
checkOutput = undefined
