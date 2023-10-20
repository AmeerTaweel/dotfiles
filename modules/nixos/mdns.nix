{...}: {
  # services.avahi = {
  #   enable = true;
  #   nssmdns = true;
  #   ipv4 = true;
  #   ipv6 = true;
  # };
  services.resolved.enable = true;
  networking.networkmanager.connectionConfig."connection.mdns" = 2;
  services.avahi.enable = true;
}
