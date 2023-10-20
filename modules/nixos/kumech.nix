{params, ...}: {
  specialisation.kumech.configuration = {
    system.nixos.tags = ["kumech"];

    # QGroundControl

    # Get full and direct access to serial ports for QGroundControl
    users.users.${params.username}.extraGroups = ["dialout"];

    # Disable modemmanager because it interferes with QGroundControl
    systemd.services.modem-manager.enable = false;
    systemd.services."dbus-org.freedesktop.ModemManager1".enable = false;

    # Raspberry Pi

    # Connect to Raspberry Pi over local Ethernet cable
    # Based on: https://github.com/NixOS/nixpkgs/issues/98050#issuecomment-1471678276
    services.resolved.enable = true;
    networking.networkmanager.connectionConfig."connection.mdns" = 2;
    services.avahi.enable = true;
  };
}
