{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Build_doctests (Component (..), Name (..), components)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import System.Environment (unsetEnv)
import Test.DocTest (doctest)

main :: IO ()
main = for_ components $ \(Component name flags pkgs sources) -> do
  putStrLn $ "Running doctests for " <> showName name
  let args = flags ++ pkgs ++ flags' ++ sources
  unsetEnv "GHC_ENVIRONMENT"
  doctest args
  where
    flags' = ["--verbose"]
    showName = \case
      NameLib l -> "lib:" <> fromMaybe "panagia" l
      NameExe e -> "exe:" <> e
