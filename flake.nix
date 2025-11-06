{
  description = "A developer shell for working on elm-integer.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          name = "elm-integer";

          packages = [
            pkgs.elmPackages.elm
            pkgs.elmPackages.elm-doc-preview
            pkgs.elmPackages.elm-format
            pkgs.elmPackages.elm-optimize-level-2
            pkgs.elmPackages.elm-test
            pkgs.elmPackages.elm-language-server
            pkgs.tailwindcss_4
            pkgs.nodejs_20
            pkgs.nodePackages.terser
            pkgs.shellcheck
          ];

          shellHook =
            ''
            export project="$PWD"
            export build="$project/.build"
            export PATH="$project/bin:$PATH"

            npm install --loglevel silent
            '';
        };
      }
    );
}

