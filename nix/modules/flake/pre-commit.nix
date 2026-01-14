# Git pre-commit hooks are defined here.

{ inputs, ... }:
{
  imports = [
    (inputs.git-hooks + /flake-module.nix)
  ];
  perSystem = { config, ... }: {
    pre-commit.settings = {
      hooks = {
        nixpkgs-fmt.enable = true;
        cabal-fmt.enable = true;
        fourmolu.enable = true;
        hlint.enable = true;
      };
    };
  };
}
