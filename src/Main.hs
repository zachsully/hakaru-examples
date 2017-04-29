import System.IO
import System.Process
import System.Exit
import System.Directory
import System.FilePath.Posix
import System.Environment

import Data.Monoid

main :: IO ()
main = do
  args <- getArgs
  let (hkc,cc) = case args of
                  (hkc:cc:[]) -> (hkc,cc)
                  _           -> error "Usage: hkc-tests <HKC> <CC>"
  tests   <- getTests
  createBuildDirectory
  tests' <- mapM (hakaruToC hkc) tests
  reportStats tests'
  tests'' <- mapM (\t -> case t of
                    Just x -> cToBinary cc x
                    Nothing -> return Nothing) tests'
  reportStats tests''
  putStrLn "Fin."

getTests :: IO [FilePath]
getTests = filter ((== ".hk") . takeExtension) <$> listDirectory "tests/hakaru"

createBuildDirectory :: IO ()
createBuildDirectory = createDirectoryIfMissing False "build"

reportStats :: [Maybe a] -> IO ()
reportStats xs = putStrLn $ "\n\nPassed " <> (show success) <> " of " <> (show total) <> "\n\n"
  where (success,total) = foldr (\x (s,t) -> case x of
                                  Just _  -> (succ s,succ t)
                                  Nothing -> (s, succ t)
                                ) (0,0) xs

hakaruToC :: String -> FilePath -> IO (Maybe FilePath)
hakaruToC hkc fp =
  let fp' = "build" </> (takeBaseName fp) <.> "c"
      process = proc hkc ["tests" </> "hakaru" </> fp
                         ,"-o"
                         ,fp']
  in
    do putStrLn $ "hkc: (" <> fp <> " , " <> fp' <> ")"
       (_,_,Just errH,pH) <- createProcess process { std_err = CreatePipe }
       exitcode <- waitForProcess pH
       case exitcode of
         ExitSuccess -> return (Just fp')
         _ -> do putStrLn "FAILED"
                 err <- hGetContents errH
                 appendFile "hakaruToC.log" ("\n" <> fp <>":\n" <> err)
                 return Nothing

cToBinary :: String -> FilePath -> IO (Maybe FilePath)
cToBinary cc fp =
  let fp' = "build" </> (takeBaseName fp) <.> "c"
      process = proc cc ["-lm","-std=c99","-pedantic",fp,"-o",fp']
  in
    do putStrLn $ "cc: (" <> fp <> " , " <> fp' <> ")"
       (_,_,Just errH,pH) <- createProcess process { std_err = CreatePipe }
       exitcode <- waitForProcess pH
       case exitcode of
         ExitSuccess -> return (Just fp')
         _ -> do putStrLn "FAILED"
                 err <- hGetContents errH
                 appendFile "hakaruToC.log" ("\n" <> fp <>":\n" <> err)
                 return Nothing

binaryToOutput :: FilePath -> IO FilePath
binaryToOutput = undefined
