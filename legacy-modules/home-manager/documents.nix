{pkgs, ...}: {
  # Zathura
  # Highly customizable and functional document viewer focused on keyboard
  # interaction.
  programs.zathura = {
    enable = true;
    options = {
      # Enable dark mode by default
      recolor = true;
      # Ability to paste selection
      selection-clipboard = "clipboard";
    };
  };
  xdg.desktopEntries.zathura = {
    type = "Application";
    name = "Zathura";
    genericName = "Document Viewer";
    exec = "${pkgs.zathura}/bin/zathura %U";
    categories = ["Application"];
    mimeType = ["application/pdf"];
  };

  # Others
  home.packages = with pkgs; [
    okular
    koreader
    libreoffice
  ];
}
