{pkgs, ...}: {
  # `discord` is unfree
  imports = [ ../nixpkgs-unfree.nix ];

  home.packages = [pkgs.discord];

  # Configure
  # Prevent Discord from trying to update itself.
  xdg.configFile.discordConfig = {
    text = ''
      {
        "SKIP_HOST_UPDATE": true
      }
    '';
    target = "discord/settings.json";
  };
}
