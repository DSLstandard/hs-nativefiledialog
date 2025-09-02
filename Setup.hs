{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Bool
import Distribution.PackageDescription qualified as PackageDescription
import Distribution.Simple qualified as Simple
import Distribution.Simple.LocalBuildInfo qualified as LocalBuildInfo
import Distribution.Simple.Program qualified as Program
import Distribution.Simple.Setup qualified as Setup
import Distribution.Simple.Utils qualified as Utils
import Distribution.Types.Flag qualified as Flag
import Distribution.Verbosity qualified as Verbosity
import Optics.Core
import Optics.TH
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

makeFieldLabelsNoPrefix ''LocalBuildInfo.LocalBuildInfo
makeFieldLabelsNoPrefix ''PackageDescription.PackageDescription

-- TIP: See https://github.com/google/btls/blob/master/Setup.hs
-- TIP: See https://github.com/deech/fltkhs/blob/master/Setup.hs

cmakeProgram = Program.simpleProgram "cmake"
ninjaProgram = Program.simpleProgram "ninja"

main :: IO ()
main = do
  let h = Simple.simpleUserHooks
  Simple.defaultMainWithHooks
    h
      { Simple.hookedPrograms =
          [ cmakeProgram
          , ninjaProgram
          ]
      , Simple.confHook = \info confFlags -> do
          -- Here, we build the '.a' library of 'nativefiledialog' with the
          -- source under './nativefiledialog-extended/'.

          -- Parse package flags
          let flags = confFlags.configConfigurationsFlags
          nfdLinuxUsePortal <- mustLookupFlag flags "nfd-linux-use-portal"

          -- TIP: To fix "[...] You can make paths relative to the package
          -- database itself by using ${pkgroot} [...]", see
          -- https://stackoverflow.com/questions/24444675/use-relative-paths-for-extra-lib-dirs-on-cabal
          cwd <- System.Directory.getCurrentDirectory
          localBuildInfo <- Simple.confHook h info confFlags
          let programDb = view #withPrograms localBuildInfo
          let tmpBuildDir = cwd </> LocalBuildInfo.buildDir localBuildInfo </> "nativefiledialog-extended-build"
          Program.runDbProgram Verbosity.verbose cmakeProgram programDb $
            [ "-G", "Ninja"
            , "-S", "nativefiledialog-extended/"
            , "-B", tmpBuildDir

            -- 'nativefiledialog' cmake flags
            , "-DNFD_PORTAL=" <> bool "OFF" "ON" nfdLinuxUsePortal
            , "-DBUILD_SHARED_LIBS=OFF"
            , "-DNFD_BUILD_SDL2_TESTS=OFF"
            , "-DNFD_BUILD_TESTS=OFF"
            ]
          Program.runDbProgram Verbosity.verbose ninjaProgram programDb ["-C", tmpBuildDir, "libnfd.a"]

          let
            lensBuildInfo = #localPkgDescr % #library % _Just % #libBuildInfo
            localBuildInfo' = localBuildInfo
              & over (lensBuildInfo % #extraLibDirs) ((tmpBuildDir </> "src") :) -- NOTE: libnfd.a is located under nativefiledialog-extended-build/src/.

          pure localBuildInfo'
      }

mustLookupFlag :: Flag.FlagAssignment -> Flag.FlagName -> IO Bool
mustLookupFlag flags name = do
  case Flag.lookupFlagAssignment name flags of
    Nothing -> do
      Utils.dieNoVerbosity $ "IMPOSSIBLE: unknown flag: " <> Flag.unFlagName name
    Just ison -> do
      pure ison

