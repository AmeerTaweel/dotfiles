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
      scope.enabled = true;
      exclude.buftypes = ["terminal" "nofile" "quickfix" "prompt" "help"];
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
