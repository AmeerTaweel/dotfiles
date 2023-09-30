{params, ...}: {
  specialisation.vtol-project.configuration = {
    system.nixos.tags = ["vtol-project"];

    # Get full and direct access to serial ports for QGroundControl
    users.users.${params.username}.extraGroups = ["dialout"];

    # Disable modemmanager because it interferes with QGroundControl
    systemd.services.modem-manager.enable = false;
    systemd.services."dbus-org.freedesktop.ModemManager1".enable = false;
  };
}
