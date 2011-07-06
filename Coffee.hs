module Coffee where
import System.FilePath ((</>), dropExtension, addExtension)
import System.Directory (doesFileExist)
import System.Posix.Files
import System.Process
import Control.Monad.Error
import System.Exit

a .&&. b = do
  a' <- a
  b' <- b
  return (a' && b')

isFile = doesFileExist
getModified f = modificationTime `fmap` getFileStatus f
cacheDir = "js/cache"
isUpToDate :: FilePath -> IO Bool
isUpToDate f = do
  e <- (isFile f .&&. isFile (outfile f))
  if not e
     then return False
     else do
       o <- getModified $ outfile f
       c <- getModified f
       return (o >= c)


outfile f = cacheDir </> (flip addExtension ".js" (dropExtension f))

compiler :: FilePath -> ErrorT String IO ()
compiler f = do
  (c, out, err) <- liftIO $ readProcessWithExitCode "coffee" ["-o", cacheDir, "-c", f] []
  case c of
       ExitSuccess -> return ()
       ExitFailure n -> throwError $ "Compile of file " ++ f ++ " failed with exit code(" ++ show n ++ "): " ++ err

-- Con: One faulty coffee file and none of the rest are compiled
compileFiles ::  [FilePath] -> ErrorT String IO ()
compileFiles xs = mapM_ compiler =<< filterM (fmap not . liftIO . isUpToDate) xs
