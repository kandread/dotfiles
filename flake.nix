{
  description = "NixOS & Nix-Darwin configuration flake for all my machines";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    nixos-hardware.url = "github:nixos/nixos-hardware/master";

    lix = {
      url = "https://git.lix.systems/lix-project/nixos-module/archive/2.91.1-2.tar.gz";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-ld = {
      url = "github:Mic92/nix-ld";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = { self, nixpkgs, nixos-hardware, lix, nix-ld, darwin, home-manager, emacs, ... }@inputs:
    let
      username = "kandread";

    in {

      nixosConfigurations = (
        import ./system/nixos {
          inherit (nixpkgs) lib;
          inherit inputs nixpkgs nixos-hardware nix-ld home-manager emacs username;
        }
      );

      darwinConfigurations = (
        import ./system/darwin {
          inherit (nixpkgs) lib;
          inherit inputs nixpkgs lix darwin home-manager emacs username;
        }
      );

    };

}
