{ pkgs, ... }:

{

  programs.kitty = {
    enable = true;
    font = {
      name = "Roboto Nerd Mono";
      size = 14;
    };
    shellIntegration.enableZshIntegration = true;
    themeFile = "ayu_mirage";
    settings = {
      scrollback_lines = 10000;
      enable_audio_bell = false;
      update_check_interval = 0;
      background_opacity = "0.9";
      strip_trailing_spaces = "smart";
      tab_bar_style = "separator";
      tab_bar_align = "center";
      tab_separator = "|";
      tab_title_template = "{fmt.fg.red}{bell_symbol}{activity_symbol}{fmt.fg.tab}{index}:{title}";
      shell = "${pkgs.zsh}/bin/zsh";
    };
  };

  programs.wezterm = {
    enable = true;
    enableZshIntegration = true;
    extraConfig = ''
      local config = {}
      config.window_close_confirmation = 'NeverPrompt'
      config.hide_tab_bar_if_only_one_tab = true
      config.audible_bell = 'Disabled'
      config.use_dead_keys = false
      config.scrollback_lines = 5000
      config.font = wezterm.font('MesloLGS Nerd Font Mono')
      config.font_size = 14
      config.front_end = 'WebGpu'
      config.window_background_opacity = 0.9
      config.enable_wayland = false
      config.color_scheme = 'Ayu Mirage (Gogh)'
      config.default_prog = { "${pkgs.zsh}/bin/zsh" }
      config.selection_word_boundary = " \t\n{}[]()\"/'`";
      return config
    '';
  };

  programs.alacritty = {
    enable = true;
    settings = {
      scrolling = {
        history = 10000;
        multiplier = 3;
      };
      window = {
        opacity = 0.9;
        option_as_alt = "Both";
      };
      font = {
        normal = {
          family = "RobotoMono Nerd Font";
          style = "Medium";
        };
        size = 14;
      };
      bell.duration = 0;
      cursor.style = "Block";
      terminal.shell.program = "${pkgs.zsh}/bin/zsh";
      keyboard.bindings = [
        { key = "Right"; mods = "Alt"; chars = "\x1BF"; }
        { key = "Left"; mods = "Alt"; chars = "\x1BB"; }
      ];
      colors = {
        primary = {
          background = "#202734";
          foreground = "#CBCCC6";
        };
        normal = {
          black = "#191E2A";
          red = "#FF3333";
          green = "#BAE67E";
          yellow = "#FFA759";
          blue = "#73D0FF";
          magenta = "#FFD580";
          cyan = "#95E6CB";
          white = "#C7C7C7";
        };
        bright = {
          black = "#686868";
          red = "#F27983";
          green = "#A6CC70";
          yellow = "#FFCC66";
          blue = "#5CCFE6";
          magenta = "#FFEE99";
          cyan = "#95E6CB";
          white = "#FFFFFF";
        };
      };
    };
  };

}
