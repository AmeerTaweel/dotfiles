{config, ...}: let
  homeDir = config.home.homeDirectory;
in {
  xdg = {
    enable = true;
    userDirs = {
      enable = true;
      createDirectories = true;
      desktop = "${homeDir}/downloads";
      documents = null;
      download = "${homeDir}/downloads";
      music = "${homeDir}/music";
      pictures = null;
      publicShare = null;
      templates = null;
      videos = null;
    };
  };
}
