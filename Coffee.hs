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
  e <- (isFile f .&&. isFile (cacheFile f))
  if not e
     then return False
     else do
       o <- getModified $ cacheFile f
       c <- getModified f
       return (o >= c)


cacheFile f = cacheDir </> (flip addExtension ".js" (dropExtension f))

compiler :: FilePath -> IO (Either String String)
compiler f = runErrorT $ do
  (c, out, err) <- liftIO $ readProcessWithExitCode "coffee" ["-o", cacheDir, "-c", f] []
  case c of
       ExitSuccess -> return $ cacheFile f
       ExitFailure n -> throwError $ "Compile of file " ++ f ++ " failed with exit code(" ++ show n ++ "): " ++ err

compileFiles ::  [FilePath] -> IO [Either String String]
compileFiles xs = mapM compiler =<< filterM (fmap not . isUpToDate) xs
