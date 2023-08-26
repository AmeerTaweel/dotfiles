{...}: {
  programs.nixvim = {
    filetype = {
      extension = {
        tmux = "tmux";
      };
    };
  };
}
