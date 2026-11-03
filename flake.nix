{
  description = "A developer shell for working on elm-integer.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=25.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        pkgsUnstable = nixpkgs-unstable.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          name = "elm-integer";

          # Elm 0.19.2 is only packaged in nixpkgs unstable. CI has no Nix and
          # uses the same versions pinned in package.json (npm `elm@0.19.2-0`,
          # `elm-test@0.19.2-1`), which npm scripts run from node_modules/.bin.
          packages = [
            pkgsUnstable.elmPackages.elm
            pkgsUnstable.elmPackages.elm-test
            pkgs.elmPackages.elm-doc-preview
            pkgs.elmPackages.elm-format
            pkgs.elmPackages.elm-optimize-level-2
            pkgs.elmPackages.elm-language-server
            # pkgs.esbuild
            pkgs.tailwindcss_4
            pkgs.nodejs_20
            pkgs.nodePackages.terser
            pkgs.shellcheck
          ];

          shellHook =
            ''
            export project="$PWD"
            export build="$project/.build"
            export PATH="$project/bin:$project/node_modules/.bin:$PATH"

            npm install --loglevel silent
            '';
        };
      }
    );
}
