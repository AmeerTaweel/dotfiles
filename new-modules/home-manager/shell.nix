{pkgs, ...}: let
  shellAliases = {
    # grep aliases
    grep = "grep --color=auto";

    # Open Tmux with UTF8 support
    tmux = "tmux -u";
  };
in {
  programs.bash = {
    enable = true;
    initExtra = ''
      # Enable Vi bindings
      set -o vi
    '';
    historyIgnore = ["ls" "cd" "exit"];
    inherit shellAliases;
  };

  programs.fish = {
    enable = true;
    shellInit = ''
      # Enable Vi bindings
      fish_vi_key_bindings
      # Turn off the greeting message
      set fish_greeting
    '';
    inherit shellAliases;
  };

  # Easy directory jumping in all shells
  programs.autojump.enable = true;

  programs.exa = {
    enable = true;
    enableAliases = true;
  };

  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
    };
  };

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
