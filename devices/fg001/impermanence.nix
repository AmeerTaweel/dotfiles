{inputs, ...}: {
  imports = [
    inputs.impermanence.nixosModules.impermanence
  ];

  boot.zfs.requestEncryptionCredentials = ["zssd/enc"];
  fileSystems."/persist-ssd".neededForBoot = true;
  fileSystems."/persist-hdd".neededForBoot = true;

  environment.persistence."/persist-ssd" = {
    enable = true;
    hideMounts = true;
    directories = [
      "/var/log"
      "/var/lib/bluetooth"
      "/var/lib/nixos"
      # We need to be able to look at coredumps even if one causes reboot
      "/var/lib/systemd/coredump"
      # WiFi Connections
      "/etc/NetworkManager/system-connections"
      # Track Power Usage History
      "/var/lib/upower"
      # Persistent SystemD Timers
      "/var/lib/systemd/timers"
      # Stateful Secrets
      "/run/secrets"
      # Tailscale State
      "/var/lib/tailscale"
      # Docker State
      "/var/lib/docker"
    ];
    files = [
      "/etc/machine-id"
    ];

    users.labmem001 = {
      directories = [
        "dotfiles"
        ".emacs.d"
        ".doom.d"
        "knowledge-base"
        ".config/BraveSoftware/Brave-Browser"
        ".config/nvim"
        {
          directory = ".ssh";
          mode = "0700";
        }
        ".local/share/direnv"
        ".config/borg"
        ".cache/borg"
        "workspace"
        ".local/share/zoxide"
        ".local/share/fish"
      ];
      files = [];
    };
  };

  environment.persistence."/persist-hdd" = {
    enable = true;
    hideMounts = true;
    directories = [];
    files = [];

    users.labmem001 = {
      directories = [
        "anime"
        "downloads"
        "pictures"
        "virtual-machines"
        "archive"
      ];
      files = [];
    };
  };

  # Never show the sudo lecture
  security.sudo.extraConfig = ''
    Defaults lecture = never
  '';
}
