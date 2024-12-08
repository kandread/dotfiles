{ pkgs, ... }:

{

  programs.zsh = {
    enable = true;
    shellAliases = {
      ls = "exa";
      tree = "exa -T";
      cat = "bat --style=plain";
      m = "mkdir -p";
      fcd = "cd $(fd --type d | fzf)";
      grep = "rg";
      ".." = "cd ..";
      "..." = "cd ../..";
    };
    history = {
      ignoreDups = true;
      size = 50000;
    };
    autocd = true;
    historySubstringSearch = {
      enable = true;
      searchUpKey = "^[[A"; #"$terminfo[kcuu1]";
      searchDownKey = "^[[B"; #"$terminfo[kcud1]";
    };
    initExtra = ''
      zstyle ":completion:*" matcher-list "m:{a-zA-Z}={A-Za-z}" "r:|[._-]=* r:|=*" "l:|=* r:|=*"
    '';
    envExtra = ''
      export WORDCHARS="*?_-.[]~=&;!#$%^(){}<>"
    '';
    completionInit = ''
    zcomet compinit
    '';
    initExtraBeforeCompInit = ''
      if [[ ! -f ~/.zcomet/bin/zcomet.zsh ]]; then
        command git clone https://github.com/agkozak/zcomet.git ~/.zcomet/bin
      fi

      source ~/.zcomet/bin/zcomet.zsh

      zcomet load zsh-users/zsh-syntax-highlighting
      zcomet load zsh-users/zsh-autosuggestions
      zcomet load agkozak/zsh-z
      zcomet load ohmyzsh plugins/ssh

      zcomet load kandread/agkozak-zsh-prompt
      AGKOZAK_PROMPT_CHAR=( ❯ ❯ ❮ )
    '';
  };

  programs.fish = {
    enable = true;
    interactiveShellInit = ''
      set -g fish_greeting
    '';
    plugins = [
      { name = "z"; src = pkgs.fishPlugins.z.src; }
      { name = "tide"; src = pkgs.fishPlugins.tide.src; }
    ];
  };

  programs.tmux = {
    enable = true;
    baseIndex = 1;
    sensibleOnTop = true;
    clock24 = true;
    disableConfirmationPrompt = true;
    historyLimit = 10000;
    customPaneNavigationAndResize = true;
    escapeTime = 0;
    extraConfig = ''
      set -g set-titles on
      set -g default-command "${pkgs.zsh}/bin/zsh"
      set-window-option -g automatic-rename
      '';
    plugins = with pkgs.tmuxPlugins; [
      yank
      jump
      pain-control
      { plugin = catppuccin;
        extraConfig = ''
          set -g @catppuccin_flavor "macchiato"
        ''; }
      { plugin = continuum;
        extraConfig = ''
          set -g @continuum-restore 'on'
          set -g @continuum-save-interval '60' # minutes
        ''; }
    ];
  };

}
