{pkgs, ...}: {
  imports = [ ./ideavim ];

  home.packages = [ pkgs.jetbrains.idea-community ];
}
