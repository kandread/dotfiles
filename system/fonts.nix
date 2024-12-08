{ pkgs, ... }:

{

  fonts.packages = with pkgs; [
    nerd-fonts.meslo-lg
    nerd-fonts.inconsolata-lgc
    nerd-fonts.roboto-mono
  ];

}
