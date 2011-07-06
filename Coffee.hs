module Coffee (cacheFile, compileFiles) where
import System.FilePath ((</>), dropExtension, addExtension)
import System.Directory (doesFileExist)
import System.Posix.Files
import System.Process
import Control.Monad.Error
import System.Exit

(.&&.)  = liftM2 (&&)

isFile = doesFileExist

getModified f = modificationTime `fmap` getFileStatus f

cacheDir = "js/cache"

outOfDate :: FilePath -> IO Bool
outOfDate = fmap not . isUpToDate
isUpToDate :: FilePath -> IO Bool
isUpToDate f = (isFile f .&&. isFile (cacheFile f)) .&&.
  liftM2 (>=) (getModified $ cacheFile f) (getModified f)

cacheFile f = cacheDir </> (flip addExtension ".js" (dropExtension f))

compiler :: FilePath -> IO (Either String String)
compiler f = runErrorT $ do
  (c, out, err) <- liftIO $ readProcessWithExitCode "coffee" ["-o", cacheDir, "-c", f] []
  case c of
       ExitSuccess -> return $ cacheFile f
       ExitFailure n -> throwError $ "Compile of file " ++ f ++ " failed with exit code(" ++ show n ++ "): " ++ err

compileFiles ::  [FilePath] -> IO [Either String String]
compileFiles xs = mapM compiler =<< filterM outOfDate xs
