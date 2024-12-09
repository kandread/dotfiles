{ inputs, ... }:

{

  programs.home-manager.enable = true;

  home = {
    username = "kandread";
    homeDirectory = "/home/kandread";
    stateVersion = "24.05";
  };

  fonts.fontconfig.enable = true;

  imports = [
    ../../user/programs.nix
    ../../user/terms.nix
    ../../user/emacs.nix
    ../../user/shells.nix
    ../../user/email.nix
    ../../user/web.nix
    ../../user/desktop.nix
    ../../user/calendar.nix
    ../../user/packages.nix
  ];

}
