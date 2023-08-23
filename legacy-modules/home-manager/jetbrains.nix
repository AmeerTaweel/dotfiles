{pkgs, ...}: {
  # Install Jetbrains packages
  home.packages = with pkgs; [
    # android-studio
    jetbrains.idea-ultimate
    # jetbrains.rider
  ];

  # Configure IdeaVIM
  # VIM Emulation inside Jetbrain's IDEs
  home.file.ideaVimRC = {
    source = ../../config/idea-vim/ideavimrc;
    target = ".ideavimrc";
  };
  xdg.configFile.ideaVimConfig = {
    source = ../../config/idea-vim/config;
    target = "idea-vim";
    recursive = true;
  };
}
