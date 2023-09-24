{...}: {
  programs.nixvim.plugins = {
    treesitter = {
      enable = true;
      folding = true;
      indent = true;
    };
    rainbow-delimiters.enable = true;
    ts-context-commentstring.enable = true;
    indent-blankline = {
      enable = true;
      buftypeExclude = ["terminal" "nofile" "quickfix" "prompt" "help"];
      useTreesitter = true;
      useTreesitterScope = false;
      showCurrentContext = true;
    };
    nvim-autopairs = {
      enable = true;
      checkTs = true;
      disableInMacro = true;
      disableInVisualblock = true;
    };
    ts-autotag.enable = true;
  };
}
