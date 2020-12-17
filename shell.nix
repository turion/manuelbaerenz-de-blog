{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  overriddenHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      http-client = self.http-client_0_7_2_1;
      essence-of-live-coding = self.essence-of-live-coding_0_2_4;
      pandoc-include-code = pkgs.haskell.lib.doJailbreak super.pandoc-include-code;
      essence-of-live-coding-warp = pkgs.haskell.lib.markUnbroken super.essence-of-live-coding-warp;
    };
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (overriddenHaskellPackages.callCabal2nix "manuelbaerenz-de-blog" ./. {});

in

  if pkgs.lib.inNixShell then drv.env else drv
