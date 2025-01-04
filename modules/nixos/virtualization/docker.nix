{params, ...}: {
  virtualisation.docker.enable = true;
  users.users.${params.username}.extraGroups = ["docker"];
}
