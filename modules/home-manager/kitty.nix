{
  config,
  inputs,
  lib,
  params,
  pkgs,
  ...
}: let
  kittyThemes = {
    ayu-dark = "Ayu";
    ayu-mirage = "Ayu Mirage";
    ayu-light = "Ayu Light";
  };
in {
  imports = [inputs.nix-colors.homeManagerModules.default];

  home.sessionVariables = lib.mkIf (params.terminal == "kitty") {
    TERMINAL = "kitty";
  };

  programs.kitty = {
    enable = true;
    settings = {
      # Enable remote control
      allow_remote_control = "yes";
      listen_on = "unix:@mykitty";

      # Disable terminal bell
      enable_audio_bell = "no";

      # Default layout is "splits"
      # Use "stack" to imitate the pane zooming feature of tmux
      enabled_layouts = "splits, stack";

      # Tab style
      tab_bar_style = "powerline";
      tab_powerline_style = "slanted";
      tab_title_template = "{index}: {title}";
    };
    keybindings = let
      # Mod Key: ctrl+a
      # To find key-codes, run:
      # kitty +kitten show_key
      mod = "ctrl+a";
    in {
      # Reload kitty.conf
      "${mod}>r" = "load_config_file";
      # Ensure that we can send ctrl-a to other apps by double ctrl-a
      "${mod}>${mod}" = "send_text all \\x01";
      # Delete word
      "ctrl+backspace" = "send_text all \\x17";

      # Windows

      # Window navigation
      "ctrl+j" = "kitten pass_keys.py bottom ctrl+j";
      "ctrl+k" = "kitten pass_keys.py top    ctrl+k";
      "ctrl+h" = "kitten pass_keys.py left   ctrl+h";
      "ctrl+l" = "kitten pass_keys.py right  ctrl+l";

      # Close the current active window
      "${mod}>x" = "close_window";
      # Zoom window
      "${mod}>z" = "toggle_layout stack";
      # Got to window
      "${mod}>g" = "focus_visible_window";
      # Swap window
      "${mod}>s" = "swap_with_window";
      # Clear window
      "${mod}>ctrl+l" = "clear_terminal scroll active";

      # Split screen with the current path
      "${mod}>-" = "launch --location=hsplit --cwd=current";
      "${mod}>/" = "launch --location=vsplit --cwd=current";

      # Split screen with the default path
      "${mod}>shift+-" = "launch --location=hsplit";
      "${mod}>shift+/" = "launch --location=vsplit";

      # Tabs

      # Next tab
      "${mod}>n" = "next_tab";
      # Previous tab
      "${mod}>p" = "previous_tab";
      # Close tab
      "${mod}>shift+x" = "close_tab";
      # Create a new tab with the current path
      "${mod}>c" = "launch --type=tab --cwd=current";
      # Create a new tab with the default path
      "${mod}>shift+c" = "launch --type=tab";
      # Rename tab
      "${mod}>," = "set_tab_title";
      # Move tab forward
      "${mod}>shift+n" = "move_tab_forward";
      # Move tab backward
      "${mod}>shift+p" = "move_tab_backward";

      # Go to tab by index
      "${mod}>1" = "goto_tab 1";
      "${mod}>2" = "goto_tab 2";
      "${mod}>3" = "goto_tab 3";
      "${mod}>4" = "goto_tab 4";
      "${mod}>5" = "goto_tab 5";
      "${mod}>6" = "goto_tab 6";
      "${mod}>7" = "goto_tab 7";
      "${mod}>8" = "goto_tab 8";
      "${mod}>9" = "goto_tab 9";
      "${mod}>0" = "goto_tab 0";
    };
    theme = kittyThemes.${config.colorScheme.slug};
  };

  xdg.configFile.vimKittyNavigatorFile1 = {
    source = "${pkgs.vimPlugins.vim-kitty-navigator}/pass_keys.py";
    target = "kitty/pass_keys.py";
  };

  xdg.configFile.vimKittyNavigatorFile2 = {
    source = "${pkgs.vimPlugins.vim-kitty-navigator}/get_layout.py";
    target = "kitty/get_layout.py";
  };
}
