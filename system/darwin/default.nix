{ inputs, nixpkgs, lix, darwin, home-manager, emacs, username, ... }:

let
  system = "aarch64-darwin";

  pkgs = import nixpkgs {
    inherit system;
    config = {
      allowUnfree = true;
    };
    overlays = [
      emacs.overlay
    ];
  };

  lib = nixpkgs.lib;

in {
  ECS-196025 = darwin.lib.darwinSystem {
    inherit system;
    specialArgs = { inherit inputs; };
    modules = [
      lix.nixosModules.default
      ./darwin.nix
      ./homebrew.nix
      ../fonts.nix
      home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.kandread = import ./home.nix;
            users.users.kandread.home = "/Users/kandread";
            home-manager.extraSpecialArgs = { inherit pkgs; };
            home-manager.backupFileExtension = "backup";
          }
    ];
};

}
