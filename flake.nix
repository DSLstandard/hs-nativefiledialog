{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hPkgs = pkgs.haskell.packages."ghc966";

        devDeps = [
          # Apparently without this, entering a nix shell breaks your VSCode terminal's bash's PS1.
          # See https://discourse.nixos.org/t/tmux-bash-prompt-breaks-inside-of-flakes/60925/4
          pkgs.bashInteractive

          pkgs.pkg-config

          hPkgs.cabal-install
          hPkgs.ghc
          hPkgs.fourmolu
          hPkgs.haskell-language-server
          hPkgs.hpack

          # For working with cbits/ and cbits_cmake/
          pkgs.clang
          pkgs.pkg-config
          pkgs.cmake 
          pkgs.ninja
        ];

        libDeps = with pkgs; [
          # zlib is used by a lot of libraries apparently.
          # (Specifically package 'warp' needs `-lz`, 'warp' is needed by `stack hoogle --server`)
          zlib

          dbus

          # SDL2 & GLFW dependencies
          SDL2
          glfw
          xorg.libX11
          xorg.libXi
          xorg.libXrandr
          xorg.libXxf86vm
          xorg.libXcursor
          xorg.libXinerama
        ];
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = devDeps ++ libDeps;
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libDeps;
        };
      });
}

