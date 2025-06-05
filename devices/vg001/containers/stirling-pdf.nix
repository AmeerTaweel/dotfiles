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
    services.stirling-pdf = {
      enable = true;
      environment = {
        SERVER_PORT = port;
        SERVER_ADDRESS = "0.0.0.0";
        SYSTEM_ENABLEANALYTICS = "false";
      };
    };
    networking.firewall.allowedTCPPorts = [port];

    # Use systemd-resolved inside the container
    # Workaround for bug https://github.com/NixOS/nixpkgs/issues/162686
    networking.useHostResolvConf = lib.mkForce false;
    services.resolved.enable = true;

    system.stateVersion = state-version;
  };
}
