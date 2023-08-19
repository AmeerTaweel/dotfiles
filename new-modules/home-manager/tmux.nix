{...}: {
  programs.tmux.enable = true;

  home.file.tmuxRC = {
    source = ../../config/tmux/rc.tmux;
    target = ".tmux.conf";
  };

  xdg.configFile.tmuxConfig = {
    source = ../../config/tmux/config;
    target = "tmux/config";
    recursive = true;
  };
}
