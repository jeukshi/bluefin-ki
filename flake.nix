{
  description = "Bluefin-ki";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', inputs', pkgs, system, config, lib, ... }: {

        haskellProjects.default = {
          devShell = {
           enable = true;
           tools = hp: { fourmolu = hp.fourmolu; };

           hlsCheck.enable = true;
          };

          autoWire = [ "packages" "apps" "checks" ];
          settings.haskell-language-server.custom = with pkgs.haskell.lib.compose; lib.flip lib.pipe [
            (disableCabalFlag "ormolu")
            # (drv: drv.override { hls-ormolu-plugin = null; })
          ];
        };
        packages.default = self'.packages.bluefin-ki;

        devShells.default = pkgs.mkShell {
          name = "bluefin-ki";
          meta.description = "bluefin-ki dev shell";
          # See https://zero-to-flakes.com/haskell-flake/devshell#composing-devshells
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            #config.treefmt.build.devShell
          ];
          nativeBuildInputs = with pkgs; [
            just
          ];

        };

      };
    };
}
