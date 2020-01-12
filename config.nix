{ packageOverrides = pkgs: {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: {
        katt = pkgs.haskell.lib.justStaticExecutables (haskellPackagesNew.callPackage ./default.nix { });
      };
    };
  };
}
