{ packageOverrides = pkgs: {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: {
        katt = haskellPackagesNew.callPackage ./default.nix { };
      };
    };
  };
}
