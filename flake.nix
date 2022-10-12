{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in rec {
      devShells.ghc810 = pkgs.mkShell {
        buildInputs = [
          pkgs.haskell.compiler.ghc810
          pkgs.haskell.packages.ghc810.cabal-install
          pkgs.haskell.packages.ghc810.haskell-language-server
        ];
      };
      devShells.ghc92 = pkgs.mkShell {
        buildInputs = [
          pkgs.haskell.compiler.ghc92
          pkgs.haskell.packages.ghc92.cabal-install
          pkgs.haskell.packages.ghc92.haskell-language-server
        ];
      };
      devShells.default = devShells.ghc810;
    });
}
