{ pkgs, config, lib, ... }:
{

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-unstable;
    extraPackages = epkgs: [
      epkgs.vterm
      epkgs.pdf-tools
      epkgs.jinx
      epkgs.treesit-grammars.with-all-grammars
      epkgs.lsp-bridge
      epkgs.mu4e
    ];
  };

}
