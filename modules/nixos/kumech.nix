{params, ...}: {
  specialisation.kumech.configuration = {
    system.nixos.tags = ["kumech"];

    # Emulation

    # EMulate aarch64-linux to be able to build Raspberry Pi configuration on a
    # more powerful device
    boot.binfmt.emulatedSystems = [
      "aarch64-linux"
    ];

    # QGroundControl

    # Get full and direct access to serial ports for QGroundControl
    users.users.${params.username}.extraGroups = ["dialout"];

    # Disable modemmanager because it interferes with QGroundControl
    systemd.services.modem-manager.enable = false;
    systemd.services."dbus-org.freedesktop.ModemManager1".enable = false;

    # Raspberry Pi

    # Connect to Raspberry Pi using hostname instead of IP
    # Based on: https://github.com/NixOS/nixpkgs/issues/98050#issuecomment-1471678276
    services.resolved.enable = true;
    networking.networkmanager.connectionConfig."connection.mdns" = 2;
    services.avahi.enable = true;

    # Enable Tailscale VPN Network
    services.tailscale.enable = true;
  };
}
