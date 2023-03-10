{
  config,
  pkgs,
  ...
}: let
  rofiThemesPath = ../../config/rofi/themes;
in {
  programs.rofi = {
    enable = true;
    cycle = true;
    terminal = config.home.sessionVariables.TERMINAL;
    theme = config.colorScheme.slug;
    plugins = with pkgs; [
      rofi-emoji
    ];
    extraConfig = {
      modi = "window,windowcd,run,drun,ssh,calc,emoji";
      matching = "fuzzy";
      kb-cancel = "Escape";
    };
  };

  xdg.configFile.rofiThemes = {
    source = rofiThemesPath;
    target = "rofi/themes";
    recursive = true;
  };
}
