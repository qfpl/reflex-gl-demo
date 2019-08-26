{ nixpkgs ? import ./nix/nixpkgs.nix {}
, compiler ? "default"
, doBenchmark ? false
}:

let
  inherit (nixpkgs) pkgs;

  codex = import ./nix/codex.nix;

  baseHaskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  haskellPackages = baseHaskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      reflex = overrideCabal super.reflex (drv: {
        broken = false;
        doCheck = false;
      });

      reflex-sdl2 = overrideCabal super.reflex-sdl2 (drv: {
        broken = false;
        jailbreak = true;
      });

      glow = super.callCabal2nix "glow" "${codex}/glow" {};
      ptrdiff = super.callCabal2nix "ptrdiff" "${codex}/ptrdiff" {};
      reflex-basic-host = super.callCabal2nix "reflex-basic-host"
        (import ./nix/reflex-basic-host.nix) {};
    };
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
in
  variant (haskellPackages.callPackage ./reflex-gl-demo.nix {})
