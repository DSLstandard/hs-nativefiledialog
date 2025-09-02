module NativeFileDialog
( -- * Initialization & Quitting
  initialize
, quit

-- * Dialog utils
, FilterItem(..)
, DialogResult(..)
, HowMany(..)
, openDialog
, saveDialog
, pickFolder

-- * Errors
, NFDError(..)
, InternalError(..)
) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Managed as Managed
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSBuilder
import Data.ByteString.Unsafe qualified as BS
import Data.Kind
import Data.Text qualified as T
import Data.Text.Foreign qualified as T
import Data.Traversable
import Foreign hiding (void)
import Foreign.C
import Foreign.Storable.Offset
import GHC.Records
import GHC.Stack
import Language.C.Inline qualified as C

C.context (C.baseCtx <> C.vecCtx)

C.include "nfd.h"
C.include "nfd_sdl2.h"
C.include "nfd_glfw3.h"

#include "nfd.h"

-- | Initialize NFD. Must be called before any other functions.
--
-- May throw a 'NFDError' if initialization fails.
initialize :: (MonadIO m) => m ()
initialize = liftIO do
  nfdresult <- {#call NFD_Init as c'NFD_Init #}
  when (toEnum (fromIntegral nfdresult) == NFDResult'Error) do
    raiseLastNFDError

-- | Terminate NFD.
quit :: MonadIO m => m ()
quit = liftIO do
  {#call NFD_Quit as c'NFD_Quit #}

-- | File open dialog.
openDialog ::
  (MonadIO m) =>
  -- | Pick a single path or multiple paths?
  HowMany result ->
  -- | Set this to the default folder that the dialog should open to (see the
  -- "Platform-specific Quirks" section for more details about the behaviour of
  -- this option on Windows)
  Maybe FilePath ->
  -- | Filter items
  [FilterItem] ->
  -- | The parent window, if any.
  Maybe ParentWindow ->
  m (DialogResult result)
openDialog howmany defaultpath filteritems parentwindow = evalManaged do
  (castPtr -> args') <- managedOpenDialogArgs defaultpath filteritems parentwindow
  liftIO $ usingOutByHowMany howmany
    (\outpath'' ->
      [C.exp| int { NFD_OpenDialogU8_With($(char** outpath''), $(void* args')) } |]
    )
    (\outpathset'' ->
      [C.exp| int { NFD_OpenDialogMultipleU8_With((const nfdpathset_t**) $(void** outpathset''), $(void* args')) } |]
    )

saveDialog ::
  MonadIO m =>
  -- | Default file name
  String ->
  -- | Set this to the default folder that the dialog should open to (see the
  -- "Platform-specific Quirks" section for more details about the behaviour of
  -- this option on Windows)
  Maybe FilePath ->
  -- | Filter items
  [FilterItem] ->
  Maybe ParentWindow ->
  m (DialogResult FilePath)
saveDialog defaultfilename defaultpath filteritems parentwindow = evalManaged do
  (castPtr -> args') <- managedSaveDialogArgs defaultfilename defaultpath filteritems parentwindow
  liftIO $ usingOutPath \outpath'' ->
    [C.exp| int { NFD_SaveDialogU8_With($(char** outpath''), $(void* args')) } |]

pickFolder ::
  (MonadIO m) =>
  -- | Pick a single path or multiple paths?
  HowMany result ->
  -- | Set this to the default folder that the dialog should open to (see the
  -- "Platform-specific Quirks" section for more details about the behaviour of
  -- this option on Windows)
  Maybe FilePath ->
  Maybe ParentWindow ->
  m (DialogResult result)
pickFolder howmany defaultpath parentwindow = evalManaged do
  (castPtr -> args') <- managedPickFolderArgs defaultpath parentwindow
  liftIO $ usingOutByHowMany howmany
    (\outpath'' ->
      [C.exp| int { NFD_PickFolderU8_With($(char** outpath''), $(void* args')) } |]
    )
    (\outpathset'' ->
      [C.exp| int { NFD_PickFolderMultipleU8_With((const nfdpathset_t**) $(void** outpathset''), $(void* args')) } |]
    )

-- # INTERNAL UTIL: NFDResult

{#enum nfdresult_t as NFDResult
  { NFD_ERROR as NFDResult'Error
  , NFD_OKAY as NFDResult'Okay
  , NFD_CANCEL as NFDResult'Cancel
  }
  deriving (Show, Eq, Ord, Bounded)
#}

-- # INTERNAL UTIL: NFDWindowHandleType

-- The native window handle type.
data NFDWindowHandleType
  = NFDWindowHandleType'Unset     -- ^ Unset window handle type
  | NFDWindowHandleType'Windows   -- ^ Windows: handle is HWND (the Windows API typedefs this to void*)
  | NFDWindowHandleType'Cocoa     -- ^ Cocoa: handle is NSWindow*
  | NFDWindowHandleType'X11       -- ^ X11: handle is Window
  -- ^ Wayland support will be implemented separately in the future
  deriving (Show, Eq, Ord, Bounded)

instance Enum NFDWindowHandleType where
  -- See src/include/nfd.h. It is defined as an unnamed enum... and I don't know
  -- how to use a #enum c2hs macro to deal with it.
  toEnum = \case
    0 -> NFDWindowHandleType'Unset
    1 -> NFDWindowHandleType'Windows
    2 -> NFDWindowHandleType'Cocoa
    3 -> NFDWindowHandleType'X11
    _ -> error "toEnum: invalid NFDWindowHandleType"

  fromEnum = \case
    NFDWindowHandleType'Unset   -> 0
    NFDWindowHandleType'Windows -> 1
    NFDWindowHandleType'Cocoa   -> 2
    NFDWindowHandleType'X11     -> 3

-- # INTERNAL UTIL: ARGS

data FilterItem = FilterItem
  { name :: T.Text
  -- ^ Display name of this filter item.
  , specs :: [T.Text]
  -- ^ File extensions to filter.
  --
  -- The list is joined with the comma character @','@ to before passing to NFD.
  --
  -- * Note: On macOS, the file dialogs do not have friendly names and there is
  --   no way to switch between filters, so the filter specifications are
  --   combined (e.g. "c,cpp,cc,h,hpp").  The filter specification is also never
  --   explicitly shown to the user.  This is usual macOS behaviour and users
  --   expect it.*
  --
  -- * Note 2: You must ensure that the specification string is non-empty and
  --   that every file extension has at least one character.  Otherwise, bad
  --   things might ensue (i.e. undefined behaviour).*
  --
  -- * Note 3: On Linux, the file extension is appended (if missing) when the
  --   user presses down the "Save" button.  The appended file extension will
  --   remain visible to the user, even if an overwrite prompt is shown and the
  --   user then presses "Cancel".*
  --
  -- * Note 4: Linux is designed for case-sensitive file filters, but this is
  --   perhaps not what most users expect.  A simple hack is used to make
  --   filters case-insensitive.  To get case-sensitive filtering, set the
  --   @NFD_CASE_SENSITIVE_FILTER@ build option to ON.*
  }
  deriving (Show, Eq, Ord)

managedOpenDialogArgs ::
  -- | Set this to the default folder that the dialog should open to (see the
  -- "Platform-specific Quirks" section for more details about the behaviour of
  -- this option on Windows)
  Maybe FilePath ->
  [FilterItem] ->
  Maybe ParentWindow ->
  Managed NFDOpenDialogArgsPtr
managedOpenDialogArgs defaultpath filteritems parentwindow = evalManaged do
  args' <- managed $ Foreign.alloca @NFDOpenDialogArgs
  setArgs'WithParentWindow args' parentwindow
  setArgs'WithDefaultPath args' defaultpath
  setArgs'WithFilter args' filteritems
  pure args'

managedSaveDialogArgs ::
  -- | Default file name
  String ->
  -- | Set this to the default folder that the dialog should open to (see the
  -- "Platform-specific Quirks" section for more details about the behaviour of
  -- this option on Windows)
  Maybe FilePath ->
  [FilterItem] ->
  Maybe ParentWindow ->
  Managed NFDSaveDialogArgsPtr
managedSaveDialogArgs defaultname defaultpath filteritems parentwindow = evalManaged do
  args' <- managed $ Foreign.alloca @NFDSaveDialogArgs
  setArgs'WithParentWindow args' parentwindow
  setArgs'WithDefaultName args' defaultname
  setArgs'WithDefaultPath args' defaultpath
  setArgs'WithFilter args' filteritems
  pure args'

managedPickFolderArgs ::
  -- | Set this to the default folder that the dialog should open to (see the
  -- "Platform-specific Quirks" section for more details about the behaviour of
  -- this option on Windows)
  Maybe FilePath ->
  Maybe ParentWindow ->
  Managed NFDPickFolderArgsPtr
managedPickFolderArgs defaultpath parentwindow = evalManaged do
  args' <- managed $ Foreign.alloca @NFDPickFolderArgs
  setArgs'WithParentWindow args' parentwindow
  setArgs'WithDefaultPath args' defaultpath
  pure args'

usingOutPath ::
  -- | @outpath'' -> NFDResultCode (CInt)@
  (Ptr (Ptr CChar) -> IO CInt) ->
  IO (DialogResult FilePath)
usingOutPath compute = evalManaged do
  outpath'' :: Ptr (Ptr CChar) <- managed $ Foreign.with nullPtr
  deferFinally do
    outpath' <- peek outpath''
    unless (outpath' == nullPtr) $ {#call NFD_FreePathU8 as c'NFD_FreePathU8 #} outpath'

  -- NOTE: NFD_OpenDialogU8_With has NFD_INLINE. We cannot use c2hs for this.
  liftIO do
    nfdresult <- compute outpath''
    case toEnum $ fromIntegral nfdresult of
      NFDResult'Error -> do
        liftIO $ DialogResult'Error <$> popLastNFDError
      NFDResult'Cancel -> do
        pure DialogResult'Cancelled
      NFDResult'Okay -> do
        outpath' <- peek outpath''
        filepath <- T.unpack <$> T.peekCString outpath'
        pure $ DialogResult'Picked filepath

usingOutPathSet ::
  -- | @outpath'' -> NFDResultCode (CInt)@
  (Ptr NFDPathSetPtr -> IO CInt) ->
  IO (DialogResult [FilePath])
usingOutPathSet compute = evalManaged do
  outpathset'' :: Ptr NFDPathSetPtr <- managed $ Foreign.with nullPtr
  deferFinally do
    outpathset' <- peek outpathset''
    unless (outpathset' == nullPtr) $ {#call NFD_PathSet_Free as c'NFD_PathSet_Free #} outpathset' 

  -- NOTE: NFD_OpenDialogMultipleU8_With has NFD_INLINE. We cannot use c2hs for this.
  liftIO do
    nfdresult <- compute outpathset''
    case toEnum (fromIntegral nfdresult) of
      NFDResult'Error -> do
        liftIO $ DialogResult'Error <$> popLastNFDError
      NFDResult'Cancel -> do
        pure DialogResult'Cancelled
      NFDResult'Okay -> do
        outpathset' <- liftIO $ peek outpathset''
        filepaths <- readPathSetToList outpathset'
        pure $ DialogResult'Picked filepaths
 where
  readPathSetToList :: MonadIO m => NFDPathSetPtr -> m [FilePath]
  readPathSetToList pathset' = evalManaged do
    numpaths' <- managed alloca

    nfdresult <- liftIO $ {#call NFD_PathSet_GetCount as c'NFD_PathSet_GetCount #} pathset' numpaths'
    when (toEnum (fromIntegral nfdresult) == NFDResult'Error) do
      raiseLastNFDError

    numpaths <- liftIO $ peek numpaths'

    outpath'' :: Ptr (Ptr CChar) <- managed alloca

    liftIO $ for [0..(numpaths-1)] \ix -> evalManaged do
      deferFinally do
        outpath' <- liftIO $ peek outpath''
        unless (outpath' == nullPtr) $ {#call NFD_FreePathU8 as c'NFD_FreePathU8 #} outpath'

      nfdresult <- liftIO $ {#call NFD_PathSet_GetPathU8 as c'NFD_PathSet_GetPathU8 #} pathset' ix outpath''
      when (toEnum (fromIntegral nfdresult) == NFDResult'Error) do
        raiseLastNFDError
      
      outpath' <- liftIO $ peek outpath''
      outpath <- liftIO $ T.unpack <$> T.peekCString outpath'
      pure outpath

-- | The result of a dialog.
data DialogResult a
  = -- | The user cancelled the dialog.
    DialogResult'Cancelled
  | -- | The user picked something.
    DialogResult'Picked a
  | -- | An NFD error occurred.
    --
    -- Note: If the user is using Linux with a DBus portal and closes the window
    -- of the file dialog (this is different from clicking the "Cancel" button),
    -- you might get the error "D-Bus file dialog interaction was ended abruptly
    -- with response code 2" through here.
    DialogResult'Error T.Text
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data HowMany :: Type -> Type where
  Single :: HowMany FilePath
  Multiple :: HowMany [FilePath]

type NFDPathSetPtr = Ptr ()

usingOutByHowMany ::
  HowMany result ->
  -- | On single. Returns NFDResultCode.
  (Ptr (Ptr CChar) -> IO CInt) ->
  -- | On multiple. Returns NFDResultCode.
  (Ptr NFDPathSetPtr -> IO CInt) ->
  IO (DialogResult result)
usingOutByHowMany Single onSingle _onMultiple = usingOutPath onSingle
usingOutByHowMany Multiple _onSingle onMultiple = usingOutPathSet onMultiple

type WindowHandle'SDL2 = Ptr ()
type WindowHandle'GLFW3 = Ptr ()

data ParentWindow
  = ParentWindow'SDL2 WindowHandle'SDL2
  | ParentWindow'GLFW3 WindowHandle'GLFW3
  deriving (Show, Eq, Ord)

setArgs'WithParentWindow ::
  forall args.
  ( HasField "parentWindow" args NFDWindowHandle
  , Offset "parentWindow" args
  ) =>
  Ptr args -> Maybe ParentWindow -> Managed ()
setArgs'WithParentWindow args' input = liftIO do
  let hdl = castPtr $ offset @"parentWindow" @args args'

  success <- case input of
    Nothing -> do
      [C.block| bool {
        nfdwindowhandle_t* hdl = (nfdwindowhandle_t *) $(void* hdl);
        hdl->type = NFD_WINDOW_HANDLE_TYPE_UNSET;
        return true;
      } |]
    Just (ParentWindow'SDL2 sdlWindow) -> do
      [C.exp| bool {
        NFD_GetNativeWindowFromSDLWindow((SDL_Window*) $(void* sdlWindow), (nfdwindowhandle_t *) $(void* hdl))
      } |]
    Just (ParentWindow'GLFW3 glfwWindow) -> do
      [C.exp| bool {
        NFD_GetNativeWindowFromGLFWWindow((GLFWwindow*) $(void* glfwWindow), (nfdwindowhandle_t *) $(void* hdl))
      } |]

  unless (toBool success) do
    raiseInternalError "failed to set parent window handle"

setArgs'WithDefaultName ::
  forall args.
  ( HasField "defaultName" args (Ptr CChar)
  , Offset "defaultName" args
  ) =>
  Ptr args -> String -> Managed ()
setArgs'WithDefaultName args' defaultname = do
  defaultname' :: Ptr CChar <- managed $ withStringUTF8NullTerm defaultname
  pokeField' @"defaultName" args' defaultname'

setArgs'WithDefaultPath ::
  forall args.
  ( HasField "defaultPath" args (Ptr CChar)
  , Offset "defaultPath" args
  ) =>
  Ptr args -> Maybe FilePath -> Managed ()
setArgs'WithDefaultPath args' defaultpath = do
  defaultpath' :: Ptr CChar <- case defaultpath of
    Just defaultpath -> managed $ withFilePathUTF8NullTerm  defaultpath
    Nothing -> pure nullPtr
  pokeField' @"defaultPath" args' defaultpath'

setArgs'WithFilter ::
  forall args.
  ( HasField "filterCount" args CUInt
  , Offset "filterCount" args
  , HasField "filterList" args NFDFilterItemPtr
  , Offset "filterList" args
  ) =>
  Ptr args -> [FilterItem] -> Managed ()
setArgs'WithFilter args' items = do
  filterlist <- for items \item -> do
    name <- managed $ withTextUTF8NullTerm item.name
    spec <- managed $ withTextUTF8NullTerm $ T.intercalate "," item.specs
    pure NFDFilterItem{name, spec}

  (filtercount, filterlist') <- managed $ withArrayLenTuple filterlist

  pokeField' @"filterCount" args' (fromIntegral filtercount)
  pokeField' @"filterList" args' filterlist'

-- INTERNAL UTIL: Errors

-- | An internal error in this library.
data InternalError = InternalError CallStack T.Text
  deriving (Show)

instance Exception InternalError

raiseInternalError :: (HasCallStack, MonadIO m) => T.Text -> m xxx
raiseInternalError msg = withFrozenCallStack $ liftIO $ throwIO $ InternalError callStack msg

-- | An error from NFD.
data NFDError = NFDError CallStack T.Text
  deriving (Show)

instance Exception NFDError

-- | Pop the last NFD error message.
popLastNFDError :: (HasCallStack, MonadIO m) => m T.Text
popLastNFDError = liftIO do
  -- FROM NFD:
  --
  -- This is set when a function returns NFD_ERROR.
  --
  -- N.B. The memory is owned by NFD and should not be freed by user code.
  --
  -- This is *always* ASCII printable characters, so it can be interpreted as
  -- UTF-8 without any conversion.
  --
  -- returns the last error that was set, or null if there is no error. */
  errstr <- {#call NFD_GetError as c'NFD_GetError#}
  when (errstr == nullPtr) $ error "INTERNAL ERROR: raiseLastNFDError: NFD_GetError returned NULL"
  errtxt <- T.peekCString errstr
  {#call NFD_ClearError as c'NFD_ClearError#}
  pure errtxt

-- | Raise the last NFD error as an exception.
raiseLastNFDError :: (HasCallStack, MonadIO m) => m xxx
raiseLastNFDError = withFrozenCallStack $ liftIO do
  errtxt <- popLastNFDError
  throwIO $ NFDError callStack errtxt

-- | Like 'runManaged' but has 'liftIO' and directly returns the output value
-- (unsafely).
evalManaged :: (MonadIO m) => Managed a -> m a
evalManaged m = liftIO $ Managed.with m pure

-- | Like 'withCString' but encodes the string as UTF-8.
withStringUTF8NullTerm :: String -> (CString -> IO args) -> IO args
withStringUTF8NullTerm string f = do
  -- TODO: THIS IS O(N). REPLACE PLACEHOLDER IMPLEMENTATION
  let bs = BS.toStrict $ BSBuilder.toLazyByteString $ BSBuilder.stringUtf8 string <> BSBuilder.word8 0
  BS.unsafeUseAsCString bs f

-- | Like 'withCString' but encodes the string as UTF-8.
withFilePathUTF8NullTerm :: FilePath -> (CString -> IO args) -> IO args
withFilePathUTF8NullTerm = withStringUTF8NullTerm

withTextUTF8NullTerm :: T.Text -> (Ptr CChar -> IO args) -> IO args
withTextUTF8NullTerm txt f = do
  -- TODO: THIS IS O(N) (See
  -- https://hackage-content.haskell.org/package/text-2.1.3/docs/src/Data.Text.Foreign.html#withCString).
  -- REPLACE PLACEHOLDER IMPLEMENTATION
  T.withCString txt $ \ptr -> f ptr

-- | Like 'withArrayLen' but returns a tuple. The type signature of this
-- function fits that of ContT.
withArrayLenTuple :: Storable a => [a] -> ((Int, Ptr a) -> IO b) -> IO b 
withArrayLenTuple xs f = withArrayLen xs \len ptr -> f (len, ptr)

-- | Run an IO action and ensure that another IO action is run afterwards,
deferFinally :: IO () -> Managed ()
deferFinally last = do
  managed_ \f -> f `finally` last

-- | Peek a field from a struct.
peekField :: forall x r a m. (HasField x r a, Offset x r, Storable a, MonadIO m) => Ptr r -> m a
peekField r' = liftIO $ peek (offset @x @r @a r')

-- | Poke a field in a struct.
pokeField' :: forall x r a m. (HasField x r a, Offset x r, Storable a, MonadIO m) => Ptr r -> a -> m ()
pokeField' r' a = liftIO $ poke (offset @x @r @a r') a

-- # NFDFilterItem

data NFDFilterItem = NFDFilterItem
  { name :: CString -- ^ in UTF8
  , spec :: CString -- ^ in UTF8
  }
  deriving (Show, Eq, Ord)

{#pointer *nfdfilteritem_t as NFDFilterItemPtr -> NFDFilterItem #}

instance Foreign.Storable.Offset.Offset "name" NFDFilterItem where
  rawOffset = {#offsetof nfdfilteritem_t->name #}
instance Foreign.Storable.Offset.Offset "spec" NFDFilterItem where
  rawOffset = {#offsetof nfdfilteritem_t->spec #}

instance Storable NFDFilterItem where
  sizeOf _ = {#sizeof nfdfilteritem_t #}
  alignment _ = {#alignof nfdfilteritem_t #}
  peek p' = do
    name <- peekField @"name" p'
    spec <- peekField @"spec" p'
    pure NFDFilterItem{..}
  poke p' NFDFilterItem{..} = do
    pokeField' @"name" p' name
    pokeField' @"spec" p' spec

-- # NFDWindowHandle

data NFDWindowHandle = NFDWindowHandle
  { type_ :: CSize            -- ^ NFD_WINDOW_HANDLE_TYPE_* enum value
  , handle :: Ptr ()          -- ^ Handle to window
  }
  deriving (Show, Eq, Ord)

instance Foreign.Storable.Offset.Offset "type_" NFDWindowHandle where
  rawOffset = {#offsetof nfdwindowhandle_t->type #}
instance Foreign.Storable.Offset.Offset "handle" NFDWindowHandle where
  rawOffset = {#offsetof nfdwindowhandle_t->handle #}

instance Storable NFDWindowHandle where
  sizeOf _ = {#sizeof nfdwindowhandle_t #}
  alignment _ = {#alignof nfdwindowhandle_t #}
  peek p' = do
    type_ <- peekField @"type_" p'
    handle <- peekField @"handle" p'
    pure NFDWindowHandle{..}
  poke p' NFDWindowHandle{..} = do
    pokeField' @"type_" p' type_
    pokeField' @"handle" p' handle

-- # NFDOpenDialogArgs

data NFDOpenDialogArgs = NFDOpenDialogArgs
  { filterList :: Ptr NFDFilterItem     -- ^ const nfdu8filteritem_t*
  , filterCount :: {#type nfdfiltersize_t #}                -- ^ nfdfiltersize_t
  , defaultPath :: Ptr CChar            -- ^ const nfdu8char_t*
  , parentWindow :: NFDWindowHandle              -- ^ nfdwindowhandle_t
  }
  deriving (Show, Eq, Ord)

{#pointer *nfdopendialogu8args_t as NFDOpenDialogArgsPtr -> NFDOpenDialogArgs #}

instance Foreign.Storable.Offset.Offset "filterList" NFDOpenDialogArgs where
  rawOffset = {#offsetof nfdopendialogu8args_t->filterList #}
instance Foreign.Storable.Offset.Offset "filterCount" NFDOpenDialogArgs where
  rawOffset = {#offsetof nfdopendialogu8args_t->filterCount #}
instance Foreign.Storable.Offset.Offset "defaultPath" NFDOpenDialogArgs where
  rawOffset = {#offsetof nfdopendialogu8args_t->defaultPath #}
instance Foreign.Storable.Offset.Offset "parentWindow" NFDOpenDialogArgs where
  rawOffset = {#offsetof nfdopendialogu8args_t->parentWindow #}

instance Storable NFDOpenDialogArgs where
  sizeOf _ = {#sizeof nfdopendialogu8args_t #}
  alignment _ = {#alignof nfdopendialogu8args_t #}
  peek p' = do
    filterList <- peekField @"filterList" p'
    filterCount <- peekField @"filterCount" p'
    defaultPath <- peekField @"defaultPath" p'
    parentWindow <- peekField @"parentWindow" p'
    pure NFDOpenDialogArgs{..}
  poke p' NFDOpenDialogArgs{..} = do
    pokeField' @"filterList" p' filterList
    pokeField' @"filterCount" p' filterCount
    pokeField' @"defaultPath" p' defaultPath
    pokeField' @"parentWindow" p' parentWindow


-- # NFDSaveDialogArgs

data NFDSaveDialogArgs = NFDSaveDialogArgs
  { filterList :: Ptr NFDFilterItem       -- ^ const nfdu8filteritem_t*
  , filterCount :: CUInt  -- ^ nfdfiltersize_t
  , defaultPath :: Ptr CChar              -- ^ const nfdu8char_t*
  , defaultName :: Ptr CChar              -- ^ const nfdu8char_t*
  , parentWindow :: NFDWindowHandle       -- ^ nfdwindowhandle_t
  }
  deriving (Show, Eq, Ord)

{#pointer *nfdsavedialogu8args_t as NFDSaveDialogArgsPtr -> NFDSaveDialogArgs #}

instance Foreign.Storable.Offset.Offset "filterList" NFDSaveDialogArgs where
  rawOffset = {#offsetof nfdsavedialogu8args_t->filterList #}
instance Foreign.Storable.Offset.Offset "filterCount" NFDSaveDialogArgs where
  rawOffset = {#offsetof nfdsavedialogu8args_t->filterCount #}
instance Foreign.Storable.Offset.Offset "defaultPath" NFDSaveDialogArgs where
  rawOffset = {#offsetof nfdsavedialogu8args_t->defaultPath #}
instance Foreign.Storable.Offset.Offset "defaultName" NFDSaveDialogArgs where
  rawOffset = {#offsetof nfdsavedialogu8args_t->defaultName #}
instance Foreign.Storable.Offset.Offset "parentWindow" NFDSaveDialogArgs where
  rawOffset = {#offsetof nfdsavedialogu8args_t->parentWindow #}

instance Storable NFDSaveDialogArgs where
  sizeOf _ = {#sizeof nfdsavedialogu8args_t #}
  alignment _ = {#alignof nfdsavedialogu8args_t #}
  peek p' = do
    filterList <- peekField @"filterList" p'
    filterCount <- peekField @"filterCount" p'
    defaultPath <- peekField @"defaultPath" p'
    defaultName <- peekField @"defaultName" p'
    parentWindow <- peekField @"parentWindow" p'
    pure NFDSaveDialogArgs{..}
  poke p' NFDSaveDialogArgs{..} = do
    pokeField' @"filterList" p' filterList
    pokeField' @"filterCount" p' filterCount
    pokeField' @"defaultPath" p' defaultPath
    pokeField' @"defaultName" p' defaultName
    pokeField' @"parentWindow" p' parentWindow

-- # NFDPickFolderArgs

data NFDPickFolderArgs = NFDPickFolderArgs
  { defaultPath :: Ptr CChar              -- ^ const nfdu8char_t*
  , parentWindow :: NFDWindowHandle       -- ^ nfdwindowhandle_t
  }
  deriving (Show, Eq, Ord)

{#pointer *nfdpickfolderu8args_t as NFDPickFolderArgsPtr -> NFDPickFolderArgs #}

instance Foreign.Storable.Offset.Offset "defaultPath" NFDPickFolderArgs where
  rawOffset = {#offsetof nfdpickfolderu8args_t->defaultPath #}
instance Foreign.Storable.Offset.Offset "parentWindow" NFDPickFolderArgs where
  rawOffset = {#offsetof nfdpickfolderu8args_t->parentWindow #}

instance Storable NFDPickFolderArgs where
  sizeOf _ = {#sizeof nfdpickfolderu8args_t #}
  alignment _ = {#alignof nfdpickfolderu8args_t #}
  peek p' = do
    defaultPath <- peekField @"defaultPath" p'
    parentWindow <- peekField @"parentWindow" p'
    pure NFDPickFolderArgs{..}
  poke p' NFDPickFolderArgs{..} = do
    pokeField' @"defaultPath" p' defaultPath
    pokeField' @"parentWindow" p' parentWindow

