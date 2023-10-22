{...}: {
  programs.tmux = {
    enable = true;
    extraConfig = ''
      # Keybindings
      source-file ${./keybindings.tmux}

      # Styles
      source-file ${./styles.tmux}

      # Settings
      source-file ${./settings.tmux}
    '';
  };
}
