{params, ...}: {
  programs.adb.enable = true;
  users.users.${params.username}.extraGroups = ["adbusers"];
}
