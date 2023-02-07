{
  config,
  pkgs,
  ...
}: let
  kittyConfigPath = ../../config/kitty/kitty.conf;
  kittyThemes = {
    ayu-dark = "Ayu";
    ayu-mirage = "Ayu Mirage";
    ayu-light = "Ayu Light";
  };
in {
  programs.kitty = {
    enable = true;
    extraConfig = builtins.readFile kittyConfigPath;
    theme = kittyThemes.${config.colorScheme.slug};
  };

  xdg.configFile.vimKittyNavigatorFile1 = with pkgs.vimPlugins; {
    source = "${vim-kitty-navigator}/pass_keys.py";
    target = "kitty/pass_keys.py";
  };

  xdg.configFile.vimKittyNavigatorFile2 = with pkgs.vimPlugins; {
    source = "${vim-kitty-navigator}/neighboring_window.py";
    target = "kitty/neighboring_window.py";
  };
}
