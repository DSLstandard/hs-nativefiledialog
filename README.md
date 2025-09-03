# `hs-nativefiledialog`

High-level Haskell bindings to
https://github.com/btzy/nativefiledialog-extended, a cross platform (Windows,
Mac, Linux) native file dialog library, and a fork of
https://github.com/mlabbe/nativefiledialog.

For in-depth details about the nativefiledialog (NFD) library and the API, you may check out
https://github.com/btzy/nativefiledialog-extended?tab=readme-ov-file#.

Example usage:
```haskell
import qualified System.Directory
import System.FilePath (FilePath)
import qualified NativeFileDialog as NFD
import qualified Data.Text as T

main :: IO ()
main = do
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
```
