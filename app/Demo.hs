{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Applicative
import NativeFileDialog qualified as NFD
import Options.Applicative qualified as Opts
import Control.Exception

data Command
  = Command'OpenDialog
      { defaultPath :: Maybe FilePath
      , multiple :: Bool
      }
  | Command'SaveDialog
      { defaultPath :: Maybe FilePath
      , defaultName :: String
      }
  | Command'PickFolder
      { defaultPath :: Maybe FilePath
      , multiple :: Bool
      }
  deriving (Show)

parseCLI :: IO Command
parseCLI =
  Opts.execParser $
    Opts.info
      (Opts.helper <*> cmd'All)
      (Opts.fullDesc <> Opts.progDesc "Demo Skottie animation viewer")
 where
  withInfo :: String -> Opts.Parser a -> Opts.ParserInfo a
  withInfo description cmd = 
    Opts.info (Opts.helper <*> cmd) (Opts.fullDesc <> Opts.progDesc description)

  cmd'All :: Opts.Parser Command
  cmd'All = Opts.subparser $ mconcat
    [ Opts.command "save-dialog" cmd'SaveDialog
    , Opts.command "open-dialog" cmd'OpenDialog
    , Opts.command "pick-folder" cmd'PickFolder
    ]

  cmd'OpenDialog :: Opts.ParserInfo Command
  cmd'OpenDialog = withInfo "Open an open dialog" do
    defaultPath <- arg'DefaultPath
    multiple <- arg'Multiple
    pure Command'OpenDialog{defaultPath, multiple}

  cmd'SaveDialog :: Opts.ParserInfo Command
  cmd'SaveDialog = withInfo "Open a save dialog" do
    defaultPath <- arg'DefaultPath
    defaultName <- arg'DefaultName
    pure Command'SaveDialog{defaultPath, defaultName}

  cmd'PickFolder :: Opts.ParserInfo Command
  cmd'PickFolder = withInfo "Open a folder-picker dialog" do
    defaultPath <- arg'DefaultPath
    multiple <- arg'Multiple
    pure Command'PickFolder{defaultPath, multiple}

  arg'DefaultPath :: Opts.Parser (Maybe FilePath)
  arg'DefaultPath = optional $ Opts.strOption $ mconcat
    [ Opts.short 'p'
    , Opts.long "default-path"
    , Opts.metavar "PATH"
    , Opts.help "Specify the default file path to start the file dialog on"
    ]

  arg'DefaultName :: Opts.Parser String
  arg'DefaultName = Opts.strOption $ mconcat
    [ Opts.short 'n'
    , Opts.long "default-name"
    , Opts.value "" 
    , Opts.metavar "NAME"
    , Opts.help "Specify the default file name to use in the dialog"
    , Opts.showDefault
    ]

  arg'Multiple :: Opts.Parser Bool
  arg'Multiple = Opts.flag False True $ mconcat
    [ Opts.short 'm'
    , Opts.long "multiple"
    , Opts.help "If set, the file dialog will allow selecting multiple files"
    ]

main :: IO ()
main = do
  cmd <- parseCLI

  putStrLn $ "CLI Input: " <> show cmd

  bracket_ NFD.initialize NFD.quit do
    case cmd of
      Command'OpenDialog{defaultPath, multiple} -> do
        if multiple
          then do
            print =<< NFD.openDialog NFD.Multiple defaultPath [] Nothing
          else do
            print =<< NFD.openDialog NFD.Single defaultPath [] Nothing

      Command'SaveDialog{defaultPath, defaultName} -> do
        print =<< NFD.saveDialog defaultName defaultPath [] Nothing

      Command'PickFolder{defaultPath, multiple} -> do
        if multiple
          then do
            print =<< NFD.pickFolder NFD.Multiple defaultPath Nothing
          else do
            print =<< NFD.pickFolder NFD.Single defaultPath Nothing
