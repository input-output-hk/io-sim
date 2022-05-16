# ###########################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib, stdenv, pkgs, haskell-nix, buildPackages, config ? { }
  # Enable profiling
, profiling ? config.haskellNix.profiling or false
, libsodium-vrf ? pkgs.libsodium-vrf }:
let
  compiler-nix-name = pkgs.localConfig.ghcVersion;
  src = haskell-nix.haskellLib.cleanGit {
    name = "io-sim-src";
    src = ../.;
  };

  projectPackages = lib.attrNames (haskell-nix.haskellLib.selectProjectPackages
    (haskell-nix.cabalProject { inherit compiler-nix-name src; }));

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject {
    inherit compiler-nix-name src;
    modules = [

      {
        # Compile all local packages with -Werror:
        packages = lib.genAttrs projectPackages
          (name: { configureFlags = [ "--ghc-option=-Werror" ]; });
      }
      {
        # Apply profiling arg to all library components in the build:
        enableLibraryProfiling = profiling;

        # Command-line options for test suites:
        packages.ouroboros-consensus-cardano-test.components.tests.test.testFlags =
          lib.mkForce [ "--no-create" ];
      }

      # Options specific to the windows cross-compiled build:
      ({ pkgs, ... }:
        lib.mkIf pkgs.stdenv.hostPlatform.isWindows {
          # Allow reinstallation of Win32
          nonReinstallablePkgs = [
            "rts"
            "ghc-heap"
            "ghc-prim"
            "integer-gmp"
            "integer-simple"
            "base"
            "deepseq"
            "array"
            "ghc-boot-th"
            "pretty"
            "template-haskell"
            # ghcjs custom packages
            "ghcjs-prim"
            "ghcjs-th"
            "ghc-boot"
            "ghc"
            "array"
            "binary"
            "bytestring"
            "containers"
            "filepath"
            "ghc-boot"
            "ghc-compact"
            "ghc-prim"
            # "ghci" "haskeline"
            "hpc"
            "mtl"
            "parsec"
            "text"
            "transformers"
            "xhtml"
            # "stm" "terminfo"
          ];
          # Make sure we use a buildPackages version of happy
          packages.pretty-show.components.library.build-tools =
            [ buildPackages.haskell-nix.haskellPackages.happy ];

          # Remove hsc2hs build-tool dependencies (suitable version will be available as part of the ghc derivation)
          packages.Win32.components.library.build-tools = lib.mkForce [ ];
          packages.terminal-size.components.library.build-tools =
            lib.mkForce [ ];
          packages.network.components.library.build-tools = lib.mkForce [ ];

        })
    ];
  };
in pkgSet
