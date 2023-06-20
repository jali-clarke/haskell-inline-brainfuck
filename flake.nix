{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }: {
    overlays = {
      haskell = haskellSelf: haskellSuper: {
        inline-brainfuck = haskellSelf.callCabal2nix "inline-brainfuck" ./. { };
      };

      default = final: prev: {
        haskellPackages = prev.haskellPackages.override { overrides = self.overlays.haskell; };
      };
    };
  } // flake-utils.lib.eachDefaultSystem (
    system:
    let
      pkgs = import nixpkgs { inherit system; overlays = [ self.overlays.default ]; };

      cabalWrapped = pkgs.writeShellScriptBin "cabal" ''
        ${pkgs.hpack}/bin/hpack && exec ${pkgs.cabal-install}/bin/cabal --offline "$@"
      '';

      format-all = pkgs.writeShellScriptBin "format-all" ''
        shopt -s globstar
        ${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt . && ${pkgs.ormolu}/bin/ormolu -i {app,src,test}/**/*.hs
      '';
    in
    rec {
      packages.default = pkgs.haskellPackages.inline-brainfuck;

      devShells.default = pkgs.mkShell {
        inputsFrom = [ packages.default.env ];
        packages = [
          cabalWrapped
          format-all
        ];
      };
    }
  );
}
