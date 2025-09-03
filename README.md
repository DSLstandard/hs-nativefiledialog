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
import qualified NativeFileDialog as NFD

main :: IO ()
main = do
  homedir <- System.Directory.getHomeDirectory

  -- Opens a file dialog for users to pick C/C++ files.
  result <- NFD.openDialog
    NFD.Multiple
    (Just homedir)
    [ NFD.FilterItem "Source Files" ["c", "cpp"]
    , NFD.FilterItem "Header Files" ["h", "hpp"]
    ]
    Nothing -- You may optionally specify the parent window of the file dialog

  case result of
    NFD.Ok files -> do
      putStrLn $ "User selected files: " <> show files
    NFD.Cancel -> do
      putStrLn "User cancelled"
    NFD.Error err -> do
      putStrLn $ "Error: " <> err
```