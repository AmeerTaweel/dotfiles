{
  params,
  pkgs,
  ...
}: {
  # Hostname
  networking.hostName = params.hostname;

  # Timezone
  time.timeZone = params.timezone;

  # Internationalisation
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "en_US.UTF-8";
      LC_IDENTIFICATION = "en_US.UTF-8";
      LC_MEASUREMENT = "en_US.UTF-8";
      LC_MONETARY = "en_US.UTF-8";
      LC_NAME = "en_US.UTF-8";
      LC_NUMERIC = "en_US.UTF-8";
      LC_PAPER = "en_US.UTF-8";
      LC_TELEPHONE = "en_US.UTF-8";
      LC_TIME = "en_US.UTF-8";
    };
  };

  # Define user account
  # Set a password with `passwd`
  users.users.${params.username} = {
    isNormalUser = true;
    description = params.name;
    extraGroups = ["networkmanager" "wheel" "video"];
    shell = pkgs.${params.shell};
  };

  # Enable popular shells
  programs = {
    fish.enable = true;
    zsh.enable = true;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = params.state-version; # Did you read the comment?
}
