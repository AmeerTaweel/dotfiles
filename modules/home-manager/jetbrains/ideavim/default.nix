{...}: {
  xdg.configFile.ideaVimConfig = {
    target = "ideavim/ideavimrc";
    text = ''
      source ${./settings.vim}
      source ${./keybindings.vim}
      source ${./plugins.vim}
    '';
  };
}
