{ pkgs, config, ...}:

{

  programs.librewolf = {
    enable = true;
  };

  programs.qutebrowser = {
    enable = true;
    extraConfig = ''
        config.source("nord-qutebrowser.py")
      '';
    settings = {
      fonts = {
        default_size = "18pt";
        web.size.minimum = 18;
      };
      colors = {
        webpage.preferred_color_scheme = "dark";
      };
    };
    keyBindings = {
      normal = {
        "<Ctrl-/>" = "hint links spawn --detach mpv {hint-url}";
      };
    };
    quickmarks =  {
      nixpkgs = "https://github.com/NixOS/nixpkgs";
      notion = "https://www.notion.so";
    };
    aliases = {
      "getbib" = "spawn --userscript getbib";
    };
  };

  programs.firefox = {
    enable = true;
  };

}
