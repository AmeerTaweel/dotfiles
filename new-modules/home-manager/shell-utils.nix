{pkgs, ...}: {
  home.packages = with pkgs; [
    # Help
    tealdeer # `tldr` command
    cht-sh

    file
    tree
    ripgrep
    fd
    curl
    bat
    ffmpeg

    zip
    unzip

    # Run arbitrary commands when files change
    entr
    # Execute a command repeatedly, and monitor the output in full-screen mode
    watch
  ];
}
