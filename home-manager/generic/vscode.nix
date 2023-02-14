{
  config,
  pkgs,
  ...
}: let
  vscode-color-theme = {
    ayu-dark = "Ayu Dark";
    ayu-mirage = "Ayu Mirage";
    ayu-light = "Ayu Light";
  };
  vscode-icon-theme = "material-icon-theme";
in {
  programs.vscode = {
    enable = true;
    package = pkgs.vscodium-fhs;
    mutableExtensionsDir = false;
    enableUpdateCheck = false;
    extensions = with pkgs.vscode-marketplace; [
      # theme
      teabyii.ayu

      # icon theme
      pkief.material-icon-theme

      # vim emulation
      vscodevim.vim

      # git
      eamodio.gitlens

      # nix language support
      jnoortheen.nix-ide
    ];
    userSettings = {
      "editor.cursorStyle" = "block";
      "editor.renderWhitespace" = "trailing";
      "editor.minimap.enabled" = false;
      "files.autoSave" = "afterDelay";
      "gitlens.hovers.currentLine.over" = "line";
      "workbench.colorTheme" = vscode-color-theme.${config.colorScheme.slug};
      "workbench.iconTheme" = vscode-icon-theme;
    };
  };
}
