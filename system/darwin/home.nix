{

  programs.home-manager.enable = true;

  home = {
    username = "kandread";
    homeDirectory = "/Users/kandread";
    stateVersion = "24.11";
  };

  fonts.fontconfig.enable = true;

  imports = [
    ../../user/programs.nix
    ../../user/terms.nix
    ../../user/emacs.nix
    ../../user/shells.nix
    ../../user/email.nix
    ../../user/calendar.nix
    ../../user/packages.nix
  ];

}
