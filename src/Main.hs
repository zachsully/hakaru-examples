import System.IO
import System.Process
import System.Exit
import System.Directory
import System.FilePath.Posix
import System.Environment

import Data.Monoid
import Data.Maybe (isJust, fromJust)

main :: IO ()
main = do
  args <- getArgs
  let (hkc,cc) = case args of
                  (hkc:cc:[]) -> (hkc,cc)
                  _           -> error "Usage: hkc-tests <HKC> <CC>"
  tests   <- getTests
  createBuildDirectory
  putStrLn $ stars <> "TESTING HKC" <> stars
  tests' <- mapM (hakaruToC hkc) tests
  putStrLn $ stars <> "TESTING CC" <> stars
  tests'' <- mapM (cToBinary cc . fromJust) . filter isJust $ tests'
  reportStats "HKC Tests" tests'
  reportStats "CC Tests" tests''
  putStrLn "Fin."

getTests :: IO [FilePath]
getTests = filter ((== ".hk") . takeExtension) <$> listDirectory "tests/hakaru"

createBuildDirectory :: IO ()
createBuildDirectory = createDirectoryIfMissing False "build"

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

hakaruToC :: String -> FilePath -> IO (Maybe FilePath)
hakaruToC hkc fp =
  let fp' = "build" </> "sea" </> (takeBaseName fp) <.> "c"
      process = proc hkc ["tests" </> "hakaru" </> fp
                         ,"-o"
                         ,fp']
  in
    do createDirectoryIfMissing False ("build" </> "sea")
       putStrLn $ "hkc: (" <> fp <> " , " <> fp' <> ")"
       (_,_,Just errH,pH) <- createProcess process { std_err = CreatePipe }
       exitcode <- waitForProcess pH
       case exitcode of
         ExitSuccess -> return (Just fp')
         _ -> do putStrLn "FAILED"
                 err <- hGetContents errH
                 appendFile "build/hkc.log" (stars <> fp <>":\n" <> err)
                 return Nothing

cToBinary :: String -> FilePath -> IO (Maybe FilePath)
cToBinary cc fp =
  let fp' = "build" </> "bin" </> (takeBaseName fp)
      process = proc cc ["-O3","-march=native","-lm","-std=c99","-pedantic"
                        ,fp,"-o",fp']
  in
    do createDirectoryIfMissing False ("build" </> "bin")
       putStrLn $ "cc: (" <> fp <> " , " <> fp' <> ")"
       (_,_,Just errH,pH) <- createProcess process { std_err = CreatePipe }
       exitcode <- waitForProcess pH
       case exitcode of
         ExitSuccess -> return (Just fp')
         _ -> do putStrLn "FAILED"
                 err <- hGetContents errH
                 appendFile "build/cc.log" (stars <> fp <>":\n" <> err)
                 return Nothing

stars :: String
stars = "\n" <> replicate 80 '*' <> "\n"

binaryToOutput :: FilePath -> IO FilePath
binaryToOutput = undefined
