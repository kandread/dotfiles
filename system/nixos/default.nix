{ inputs, nixpkgs, lib, lix, nixos-hardware, home-manager, emacs, username, ... }:

let
  system = "x86_64-linux";

  pkgs = import nixpkgs {
    inherit system;
    config = {
      allowUnfree = true;
      joypixels.acceptLicense = true;
    };
    overlays = [
      emacs.overlay
      (final: prev: {
        zoom-us = prev.zoom-us.overrideAttrs (attrs: {
          nativeBuildInputs = (attrs.nativeBuildInputs or []) ++ [ pkgs.bbe ];
          postFixup = ''
    cp $out/opt/zoom/zoom .
    bbe -e 's/\0manjaro\0/\0nixos\0\0\0/' < zoom > $out/opt/zoom/zoom
  '' + (attrs.postFixup or "") + ''
    sed -i 's|Exec=|Exec=env XDG_CURRENT_DESKTOP="gnome" |' $out/share/applications/Zoom.desktop
  '';
        });
      })
    ];
  };

  homeConfig = hmc: {
    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;
    home-manager.users.kandread = import hmc;
    home-manager.extraSpecialArgs = { inherit system inputs; };
    home-manager.backupFileExtension = "backup";
  };

  nixosModules = [
    lix.nixosModules.default
    home-manager.nixosModules.home-manager
    (homeConfig ./home.nix)
  ];
in {

  amdgland = lib.nixosSystem {
    inherit pkgs system;
    modules = nixosModules ++ [
      nixos-hardware.nixosModules.lenovo-thinkpad-t495
      ../../hosts/amdgland
    ];
  };

  workgland = lib.nixosSystem {
    inherit pkgs system;
    modules = nixosModules ++ [
      ../../hosts/workgland
    ];
  };

  theligland = lib.nixosSystem {
    inherit pkgs system;
    modules = nixosModules ++ [
      ../../hosts/theligland
    ];
  };

  nucgland = lib.nixosSystem {
    inherit pkgs system;
    modules = nixosModules ++ [
      ../../hosts/nucgland
    ];
  };

}
