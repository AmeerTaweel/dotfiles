{
  host-address,
  local-address,
  port,
  state-version,
  ...
}: {
  autoStart = true;
  privateNetwork = true;
  hostAddress = host-address;
  localAddress = local-address;
  config = {
    config,
    lib,
    pkgs,
    ...
  }: {
    nix.enable = false;

    services.scrutiny = {
      enable = true;
      collector.enable = false;
      influxdb.enable = true;
      settings.web.listen.host = "0.0.0.0";
      settings.web.listen.port = port;
    };
    networking.firewall.allowedTCPPorts = [port];

    # Use systemd-resolved inside the container
    # Workaround for bug https://github.com/NixOS/nixpkgs/issues/162686
    networking.useHostResolvConf = lib.mkForce false;
    services.resolved.enable = true;

    system.stateVersion = state-version;
  };
}
