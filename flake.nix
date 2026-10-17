{
  description = "tvshow";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs :
    inputs.flake-utils.lib.eachDefaultSystem (system:
     let pkgs = inputs.nixpkgs.legacyPackages.${system};
         haskell = pkgs.haskell.packages.ghc98;

     in
  {
    packages = {
      default = (haskell.callCabal2nix "tvshow" ./. {}).overrideAttrs (old: {
        nativeBuildInputs = (old.nativeBuildInputs or []) ++ [
          pkgs.makeWrapper
        ];

        postFixup = (old.postFixup or "") + ''
          wrapProgram $out/bin/tvshow \
            --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.ffmpeg ]}
        '';
      });
    };
    devShells = {
      default = pkgs.mkShell {
        packages = [
          haskell.ghc
          pkgs.cabal-install
          pkgs.ffmpeg
          pkgs.stylish-haskell
          pkgs.zlib
        ];
      };
    };
  });
}
