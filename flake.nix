{
  inputs = {
    # nixpkgs.url = "github:nixos/nixpkgs/2e87d165f74411ae00f964a508945696969ff53d";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    haskell-flake.url = "github:srid/haskell-flake";
    flake-parts.url = "github:hercules-ci/flake-parts";
    systems.url = "github:nix-systems/default";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, ... }: {

        # Typically, you just want a single project named "default". But
        # multiple projects are also possible, each using different GHC version.
        haskellProjects.default = {
          # The base package set representing a specific GHC version.
          # By default, this is pkgs.haskellPackages.
          # You may also create your own. See https://zero-to-flakes.com/haskell-flake/package-set
          # basePackages = pkgs.haskell.packages.ghc8107;
          # packages = {
          #   clay.source = "0.14.0";
          #   optparse-applicative.source = "0.16.1.0";
          #   pretty-simple.source = "4.0.0.0";
          # };

          # Extra package information. See https://zero-to-flakes.com/haskell-flake/dependency
          #
          # Note that local packages are automatically included in `packages`
          # (defined by `defaults.packages` option).
          #
          # packages = { 
          #   aeson.source = "1.5.0.0"; # Hackage version override
          #   shower.source = inputs.shower; 
          # };

          # devShell = {
          #  # Enabled by default
          #  enable = true;  
          #
          #  # Programs you want to make available in the shell.
          #  # Default programs can be disabled by setting to 'null'
          #  tools = hp: { fourmolu = hp.fourmolu; ghcid = null; };
          #
          #  hlsCheck.enable = true;
          # };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.weeder;
      };
    };
}