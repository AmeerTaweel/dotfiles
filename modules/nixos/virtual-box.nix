{
  params,
  pkgs,
  ...
}: {
  virtualisation.virtualbox.host.enable = true;
  users.users.${params.username}.extraGroups = ["vboxusers"];
}
