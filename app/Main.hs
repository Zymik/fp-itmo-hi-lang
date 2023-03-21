{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Arrow (left)
import Control.Exception
import Control.Monad (join)
import Data.Set
import HW3.Action
import HW3.Base (HiValue)
import HW3.Evaluator (eval)
import HW3.Parser
import HW3.Pretty (prettyValue)
import Prettyprinter.Render.Terminal (putDoc, Color(Red), color)
import System.Console.Haskeline
import Prettyprinter (Pretty(pretty), (<+>), annotate )
import Control.Monad.Cont (lift)
import Text.Megaparsec (errorBundlePretty)

permissions :: Set HiPermission
permissions = fromList [minBound .. maxBound]

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing -> lift $ putStrLn ""
        Just input ->
          do
            case parse input of
              Left e -> lift $ printError $ errorBundlePretty e
              Right parsed ->
                do
                  let evaluation = left show <$> runHIO (eval parsed) permissions
                  withHandling <- lift $ try @SomeException evaluation
                  printResult (join $ left show withHandling)
                  lift $ putStrLn ""          
      loop

printResult :: Either String HiValue -> InputT IO ()
printResult evaluated = case evaluated of
  Left e   -> lift $ printError e
  Right hv -> lift $ putDoc (prettyValue hv)

printError :: String -> IO ()
printError e = putDoc ( annotate (color Red) (pretty "Error:" <+> pretty e))