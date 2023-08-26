{config, ...}: {
  xdg.configFile.ideaVimRC = {
    target = "ideavim/ideavimrc";
    text = ''
      source ${config.xdg.configHome}/ideavim/settings.vim
      source ${config.xdg.configHome}/ideavim/keybindings.vim
      source ${config.xdg.configHome}/ideavim/plugins.vim
    '';
  };

  xdg.configFile.ideaVimSettings = {
    source = ./settings.vim;
    target = "ideavim/settings.vim";
  };

  xdg.configFile.ideaVimKeybindings = {
    source = ./keybindings.vim;
    target = "ideavim/keybindings.vim";
  };

  xdg.configFile.ideaVimPlugins = {
    source = ./plugins.vim;
    target = "ideavim/plugins.vim";
  };
}
