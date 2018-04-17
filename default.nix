{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  haskellPackagesOrig = if compiler == "default"
                        then pkgs.haskellPackages
                        else pkgs.haskell.packages.${compiler};

  haskellPackages = haskellPackagesOrig.override {
    overrides = self: super: {
      genvalidity-hspec        = pkgs.haskell.lib.dontCheck super.genvalidity-hspec;
      genvalidity-hspec-cereal = pkgs.haskell.lib.dontCheck super.genvalidity-hspec-cereal;
      genvalidity-hspec-aeson  = pkgs.haskell.lib.dontCheck super.genvalidity-hspec-aeson;

      grenade = pkgs.haskell.lib.overrideCabal
        (super.callPackage (super.haskellSrc2nix {
          name = "grenade";
          src = ./grenade.cabal;
        }) {})
        (_drv: {src = ./.;});
    };
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant haskellPackages.grenade;

in

  if pkgs.lib.inNixShell then drv.env else drv
