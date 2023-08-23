{
  config,
  pkgs,
  ...
}: let
  alacrittyConfigPath = ../../config/alacritty;
in {
  programs.alacritty.enable = true;

  xdg.configFile.alacrittyConfig = {
    source = alacrittyConfigPath;
    target = "alacritty";
    recursive = true;
  };

  xdg.configFile.alacrittyTheme = {
    text = "import:\n  - ~/.config/alacritty/themes/${config.colorScheme.slug}.yml";
    target = "alacritty/theme.yml";
  };
}
