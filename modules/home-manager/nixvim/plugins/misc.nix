{pkgs, ...}: {
  programs.nixvim = {
    plugins = {
      surround.enable = true;
      lualine = {
        enable = true;
        globalstatus = true;
        iconsEnabled = true;
      };
      comment-nvim.enable = true;
      gitsigns = {
        enable = true;
        currentLineBlame = true;
      };
      todo-comments = {
        enable = true;
        signs = false;
      };
      vim-matchup = {
        enable = true;
        enableSurround = true;
      };

      which-key.enable = true;
    };
    extraPlugins = with pkgs.vimPlugins; [
      # Navigation
      vim-kitty-navigator

      # Dynamically set buffer indentation
      vim-sleuth

      # Provide additional text objects
      # Cheatsheet for targets-vim in the link below:
      # https://github.com/wellle/targets.vim/blob/master/cheatsheet.md
      targets-vim

      # Helpers for UNIX
      vim-eunuch

      # Enable repeating supported plugin maps with "."
      vim-repeat

      # Make the yanked region apparent
      vim-highlightedyank
    ];
  };
}
