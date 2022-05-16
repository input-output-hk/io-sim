# our packages overlay
pkgs: _:
with pkgs; {
  ioSimHaskellPackages = import ./io-sim.nix {
    inherit config pkgs lib stdenv haskell-nix buildPackages;
  };

  cabal = haskell-nix.tool localConfig.ghcVersion "cabal" {
    version = "latest";
    inherit (ioSimHaskellPackages) index-state;
  };

  stylish-haskell = haskell-nix.tool localConfig.ghcVersion "stylish-haskell" {
    version = "0.13.0.0";
    inherit (ioSimHaskellPackages) index-state;
  };

  trace = builtins.trace;
}
