{config, ...}: let
  homeDir = config.home.homeDirectory;
in {
  xdg = {
    enable = true;
    userDirs = {
      enable = true;
      createDirectories = true;
      desktop = null;
      documents = null;
      download = "${homeDir}/downloads";
      music = null;
      pictures = null;
      publicShare = null;
      templates = null;
      videos = null;
    };
  };
}
