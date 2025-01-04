{pkgs, ...}: {
  imports = [./ideavim];

  home.packages = [
    (pkgs.jetbrains.plugins.addPlugins pkgs.jetbrains.idea-community [
      "ideavim"
    ])
  ];
}
