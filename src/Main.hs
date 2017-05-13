import Data.Monoid
import Data.Maybe (isJust, fromJust)
-- import Options.Applicative
import System.IO
import System.Process
import System.Exit
import System.Directory
import System.FilePath.Posix
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let (hkc,cc) = case args of
                  (hkc:cc:[]) -> (hkc,cc)
                  _           -> error "Usage: hkc-tests <HKC> <CC>"

  tests   <- getTests
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

-- Check to see if it produces legal C code
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

-- we will want the type of the program for testing output
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
