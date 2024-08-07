{
  description = "my project description";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = nixpkgs.legacyPackages.${system}; in
        {
          devShells.default = 
            pkgs.mkShell {
              buildInputs = with pkgs; [
                haskellPackages.cabal-install
                haskell-language-server
                ghc
              ];
            };
        }
      );
}
