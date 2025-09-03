module Main where

import Control.Exception
import qualified Data.Text as T
import qualified NativeFileDialog as NFD
import qualified System.Directory

main :: IO ()
main = bracket_ NFD.initialize NFD.quit do
  homedir <- System.Directory.getHomeDirectory

  -- Opens a file dialog for users to pick C/C++ files.
  result :: NFD.DialogResult [FilePath] <- NFD.openDialog
    NFD.Multiple
    (Just homedir)
    [ NFD.FilterItem "Source Files" ["c", "cpp"]
    , NFD.FilterItem "Header Files" ["h", "hpp"]
    ]
    Nothing -- You may optionally specify the parent window of the file dialog

  case result of
    NFD.DialogResult'Picked files -> do
      putStrLn $ "User selected files: " <> show files
    NFD.DialogResult'Cancelled -> do
      putStrLn "User cancelled"
    NFD.DialogResult'Error err -> do
      putStrLn $ "Error: " <> T.unpack err
