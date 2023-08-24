{...}: {
  programs.nixvim = {
    autoGroups = {
      AUTO_REBALANCE.clear = true;
      DISABLE_AUTO_COMMENT.clear = true;
      SMART_LINE_NUMBERS.clear = true;
      DISABLE_NETRW_LIST.clear = true;
      DISABLE_AUTO_FOLD.clear = true;
    };
    autoCmd = [
      {
        # Automatically re-balance windows on resize
        group = "AUTO_REBALANCE";
        event = ["VimResized"];
        pattern = ["*"];
        command = "wincmd =";
      }
      {
        # Disable auto-commenting
        group = "DISABLE_AUTO_COMMENT";
        event = ["FileType"];
        pattern = "*";
        command = "setlocal formatoptions-=c formatoptions-=r formatoptions-=o";
      }
      {
        # Make line numbers absolute when in insert mode and on buffer leaving
        group = "SMART_LINE_NUMBERS";
        event = ["BufLeave" "FocusLost" "InsertEnter"];
        pattern = "*";
        command = "setlocal norelativenumber";
      }
      {
        # Restore relative line numbers when leaving insert move or entering buffer
        group = "SMART_LINE_NUMBERS";
        event = ["BufEnter" "FocusGained" "InsertLeave"];
        pattern = "*";
        command = "setlocal relativenumber";
      }
      {
        # Disable annoying folds when opening a file
        group = "DISABLE_AUTO_FOLD";
        event = ["BufWinEnter"];
        pattern = "*";
        command = "normal zR";
      }
      {
        # Disable whitespace character hints in netrw
        group = "DISABLE_NETRW_LIST";
        event = ["FileType"];
        pattern = "netrw";
        command = "setlocal nolist";
      }
    ];
  };
}
