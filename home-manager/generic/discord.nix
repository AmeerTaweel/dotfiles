{pkgs, ...}: {
  # Install
  home.packages = [pkgs.discord];

  # AutoStart
  autostart.discord = {
    description = "Autostart Discord desktop client";
    exec = "${pkgs.discord}/bin/discord";
  };

  # Configure
  # Prevent Discord from trying to update itself.
  xdg.configFile.discordConfig = {
    text = ''      {
            "SKIP_HOST_UPDATE": true
          }'';
    target = "discord/settings.json";
  };
}
