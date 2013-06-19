
module Language.Egison
       ( module Language.Egison.Types
       , module Language.Egison.Parser
       , module Language.Egison.Primitives
       , version
       , counter --danger
       , fromEgisonM
       , defaultEnv
       , runEgisonFile
       , runEgisonTopExpr
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error

import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Data.Version
import qualified Paths_egison as P

import System.FilePath

import Language.Egison.Types
import Language.Egison.Parser
import Language.Egison.Primitives
import Language.Egison.Core

version :: Version
version = P.version

-- Unsafe
counter :: IORef Int
counter = unsafePerformIO (newIORef 0)

readCounter :: IO Int
readCounter = readIORef counter

updateCounter :: Int -> IO ()
updateCounter = writeIORef counter

modifyCounter :: FreshT IO a -> IO a
modifyCounter m = do
  seed <- readCounter
  (result, seed) <- runFreshT seed m 
  updateCounter seed
  return result  

fromEgisonM :: EgisonM a -> IO (Either EgisonError a)
fromEgisonM = modifyCounter . runEgisonM

defaultEnv :: IO (Env, ModuleEnv)
defaultEnv = do
  env <- primitiveEnv
  result <- modifyCounter . runEgisonM $ do
    moduleEnv <- loadCoreLibraries env nullModuleEnv
    env' <- importCoreLibraries env moduleEnv
    return (env', moduleEnv)
  case result of
    Left e -> do
      print e
      putStrLn "failed to load the core libraries"
      return (env, nullModuleEnv)
    Right r -> return r

runEgisonTopExpr :: (Env, ModuleEnv) -> String -> IO (Either EgisonError (Env, ModuleEnv))
runEgisonTopExpr (env, moduleEnv) input =
  modifyCounter $ runEgisonM $ readTopExpr input >>= evalTopExpr env moduleEnv

runEgisonFile :: FilePath -> [String] -> IO (Either EgisonError ())
runEgisonFile file args = modifyCounter . runEgisonM $ do
  let moduleName = dropExtension file
  env <- liftIO primitiveEnv
  moduleEnv <- loadCoreLibraries env nullModuleEnv
  moduleEnv' <- loadModule env moduleEnv moduleName
  evalTopExprs nullEnv moduleEnv' [(Import moduleName Nothing), (Execute args)]
  return ()
