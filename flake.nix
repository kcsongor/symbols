{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils/cfacdce06f30d2b68473a46042957675eebb3401";
    nixpkgs.url = "github:NixOS/nixpkgs/1fb781f4a148c19e9da1d35a4cbe15d0158afc4e";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};

      packageName = "symbols";
      override = { overrides = self: super: { "${packageName}" = self.callCabal2nix packageName ./. { }; }; };
      ghcVersion = "ghc927";
      hpkgs = pkgs.haskell.packages.${ghcVersion};
      getHaskellPackagesDeps = someHaskellPackages: with pkgs.lib.lists; (subtractLists someHaskellPackages (concatLists (map (package: concatLists (__attrValues package.getCabalDeps)) someHaskellPackages)));
      ghcForPackages = hpkgs_: override_: localHaskellPackageNames: (hpkgs_.override override_).ghcWithPackages (ps: getHaskellPackagesDeps (map (x: ps.${x}) localHaskellPackageNames));
      ghc = ghcForPackages hpkgs override [ packageName ];

      tools = [
        pkgs.cabal-install
        # ghc should go before haskell-language-server - https://github.com/NixOS/nixpkgs/issues/225895
        ghc
        hpkgs.haskell-language-server
      ];

      devShells.default = pkgs.mkShell {
        buildInputs = tools;
      };
    in
    {
      inherit devShells;
    });
}
