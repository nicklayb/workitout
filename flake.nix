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

        # Nixpkgs only packages Elm 0.19.1, so the official 0.19.3 release
        # binary is fetched with a fixed hash. It is byte-identical to the one
        # shipped by the `elm@0.19.3-0` npm package pinned in package.json.
        elmVersion = "0.19.3";

        elmReleaseAssetBySystem = {
          x86_64-linux = { asset = "linux-x64"; hash = "sha256-0RcCsxV37YtyC52BxSyKGaUrcrH8+t7w/K2S8heYxYo="; };
          aarch64-linux = { asset = "linux-arm"; hash = "sha256-TSLE0Q+J5ILTVUt69PZSf4oF5NgmSKB7I/HyEUlbXSQ="; };
          x86_64-darwin = { asset = "mac-x64"; hash = "sha256-SpxVeOU8T+tVuRyO6aD0bamRNBTGMD0cVa3Q06vNmSE="; };
          aarch64-darwin = { asset = "mac-arm"; hash = "sha256-pCbkfRTLoFiKqBR1rcMroku/BFIJ0MVg0KiaXE88NNA="; };
        };

        elmReleaseAsset = elmReleaseAssetBySystem.${system};

        elm = pkgs.stdenvNoCC.mkDerivation {
          pname = "elm";
          version = elmVersion;

          src = pkgs.fetchurl {
            url = "https://github.com/elm/compiler/releases/download/${elmVersion}/elm-${elmVersion}-${elmReleaseAsset.asset}.gz";
            hash = elmReleaseAsset.hash;
          };

          dontUnpack = true;

          # The Linux release binaries are statically linked, nothing to patch.
          installPhase = ''
            runHook preInstall
            mkdir -p $out/bin
            gunzip -c $src > $out/bin/elm
            chmod +x $out/bin/elm
            runHook postInstall
          '';
        };
      in
      {
        packages.elm = elm;

        devShells.default = pkgs.mkShell {
          name = "elm-integer";

          # elm and elm-test are pinned in package.json (npm `elm@0.19.3-0`,
          # `elm-test@0.19.3-0`) and exposed through node_modules/.bin, so CI
          # and the dev shell use the exact same versions. Nixpkgs' elm-test
          # is 0.19.1-only and must not be used.
          packages = [
            elm
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
