let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          katt = haskellPackagesNew.callPackage ./default.nix { };

          req = haskellPackagesNew.callPackage ./req.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { katt = pkgs.haskellPackages.katt; }
