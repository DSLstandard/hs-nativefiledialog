{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Text qualified as T
import Foreign
import qualified Bindings.GLFW as GLFW
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.C as GLFW
import qualified NativeFileDialog as NFD
import qualified Options.Applicative as Opts
import qualified SDL
import qualified SDL.Internal.Types
import qualified Text.Read

data ParentWindowType
  = ParentWindowType'None
  | ParentWindowType'SDL2
  | ParentWindowType'GLFW3
  deriving (Show, Eq, Ord, Enum, Bounded)

data Command
  = Command'OpenDialog
      { defaultPath :: Maybe FilePath
      , multiple :: Bool
      , parentWindow :: ParentWindowType
      , filterList :: [NFD.FilterItem]
      }
  | Command'SaveDialog
      { defaultPath :: Maybe FilePath
      , defaultName :: String
      , parentWindow :: ParentWindowType
      , filterList :: [NFD.FilterItem]
      }
  | Command'PickFolder
      { defaultPath :: Maybe FilePath
      , multiple :: Bool
      , parentWindow :: ParentWindowType
      }
  deriving (Show)

parseCLI :: IO Command
parseCLI =
  Opts.execParser $
    Opts.info
      (Opts.helper <*> cmd'All)
      (Opts.fullDesc <> Opts.progDesc "A CLI demo for hs-nativefiledialog")
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
    parentWindow <- arg'ParentWindow
    filterList <- arg'FilterItems
    pure Command'OpenDialog{defaultPath, multiple, parentWindow, filterList}

  cmd'SaveDialog :: Opts.ParserInfo Command
  cmd'SaveDialog = withInfo "Open a save dialog" do
    defaultPath <- arg'DefaultPath
    defaultName <- arg'DefaultName
    parentWindow <- arg'ParentWindow
    filterList <- arg'FilterItems
    pure Command'SaveDialog{defaultPath, defaultName, parentWindow, filterList}

  cmd'PickFolder :: Opts.ParserInfo Command
  cmd'PickFolder = withInfo "Open a folder-picker dialog" do
    defaultPath <- arg'DefaultPath
    multiple <- arg'Multiple
    parentWindow <- arg'ParentWindow
    pure Command'PickFolder{defaultPath, multiple, parentWindow}

  arg'ParentWindow :: Opts.Parser ParentWindowType
  arg'ParentWindow =
    asum
      [ Opts.flag' ParentWindowType'SDL2 $ mconcat
        [ Opts.long "sdl2"
        , Opts.help "Used for testing. Spawns a SDL2 window as the parent window of the file dialog"
        ]
      , Opts.flag' ParentWindowType'GLFW3 $ mconcat
        [ Opts.long "glfw3"
        , Opts.help "Used for testing. Spawns a GLFW3 window as the parent window of the file dialog"
        ]
      , pure ParentWindowType'None
      ]

  arg'DefaultPath :: Opts.Parser (Maybe FilePath)
  arg'DefaultPath = optional $ Opts.strOption $ mconcat
    [ Opts.short 'p'
    , Opts.long "default-path"
    , Opts.metavar "PATH"
    , Opts.help "Specify the default file path to start the file dialog on"
    ]

  arg'FilterItems :: Opts.Parser [NFD.FilterItem]
  arg'FilterItems =
    asum
      [ Opts.option filterListReader $ mconcat
          [ Opts.short 'f'
          , Opts.long "filter"
          , Opts.metavar "FILTER"
          , Opts.help "Specify a list of filter items. The input is parsed as Haskell expression of type [(Text, [Text])]. Usage: -f '[(\"Source Files\", [\"c\", \"cpp\"]), (\"Header Files\", [\"h\", \"hpp\"])]'"
          ]
      , pure []
      ]
   where
    filterListReader :: Opts.ReadM [NFD.FilterItem]
    filterListReader = Opts.eitherReader \input -> do
      case Text.Read.readMaybe @[(T.Text, [T.Text])] input of
        Nothing -> do
          Left $ "Failed to parse filter list: " <> input
        Just items -> do
          Right $ fmap (\(name, specs) -> NFD.FilterItem{name, specs}) items

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

-- | Helper function to create a parent window of the specified type, run the
-- given action with the parent window, and then clean up the parent window.
withParentWindow :: ParentWindowType -> (Maybe NFD.ParentWindow -> IO r) -> IO r
withParentWindow ParentWindowType'None continue =
  continue Nothing
withParentWindow ParentWindowType'SDL2 continue =
  bracket_
    SDL.initializeAll
    SDL.quit
    ( do
        win :: SDL.Window <-
          SDL.createWindow "SDL2 window" SDL.defaultWindow
            { SDL.windowVisible = True
            , SDL.windowResizable = True
            }
        let SDL.Internal.Types.Window (winhdl :: Ptr ()) = win
        result <- continue (Just (NFD.ParentWindow'SDL2 winhdl))
        SDL.destroyWindow win
        pure result
    )
withParentWindow ParentWindowType'GLFW3 continue =
  bracket_
    ( do
        ok <- GLFW.init
        unless ok do
          error "Failed to initialize GLFW"
    )
    GLFW.terminate
    ( do
        win :: GLFW.Window <- GLFW.createWindow 640 480 "GLFW window" Nothing Nothing >>= \case
          Nothing -> error "Failed to create GLFW window"
          Just w -> pure w
        let winhdl :: Ptr GLFW.C'GLFWwindow = GLFW.toC win
        let winhdl' :: Ptr () = castPtr winhdl
        result <- continue (Just (NFD.ParentWindow'GLFW3 winhdl'))
        GLFW.destroyWindow win
        pure result
    )

main :: IO ()
main = do
  cmd <- parseCLI

  putStrLn $ "CLI Input: " <> show cmd

  bracket_ NFD.initialize NFD.quit do
    withParentWindow (parentWindow cmd) \parentwin -> do
      case cmd of
        Command'OpenDialog{defaultPath, multiple, filterList} -> do
          if multiple
            then do
              print =<< NFD.openDialog NFD.Multiple defaultPath filterList parentwin
            else do
              print =<< NFD.openDialog NFD.Single defaultPath filterList parentwin

        Command'SaveDialog{defaultPath, defaultName, filterList} -> do
          print =<< NFD.saveDialog defaultName defaultPath filterList parentwin

        Command'PickFolder{defaultPath, multiple} -> do
          if multiple
            then do
              print =<< NFD.pickFolder NFD.Multiple defaultPath parentwin
            else do
              print =<< NFD.pickFolder NFD.Single defaultPath parentwin
