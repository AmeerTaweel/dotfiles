{...}: {
  programs.nixvim = {
    maps = {
      normal = {
        # Window Splits
        "<leader>-" = {
          action = "<cmd>split<cr>";
          desc = "split window horizontally";
        };
        "<leader>/" = {
          action = "<cmd>vsplit<cr>";
          desc = "split window vertically";
        };

        # Tabs
        "<leader>tc" = {
          action = "<cmd>tabclose<cr>";
          desc = "close tab";
        };
        "<leader>tn" = {
          action = "<cmd>tabnew<cr>";
          desc = "new tab";
        };

        # Misc
        "<cr>" = {
          action = "o<esc>";
          desc = "insert new line";
        };
        "<space>" = {
          action = ",";
          desc = "repeat last f/t/F/T in opposite direction";
        };
        "<leader>th" = {
          action = "<cmd>noh<cr>";
          desc = "remove last search highlights";
        };

        # System Clipboard
        "<leader>d" = {
          action = "\"+d";
          desc = "delete to clipboard";
        };
        "<leader>D" = {
          action = "\"+d$";
          desc = "delete (untill line end) to clipboard";
        };
        "<leader>p" = {
          action = "\"+p";
          desc = "paste from clipboard";
        };
        "<leader>P" = {
          action = "\"+P";
          desc = "paste from clipboard";
        };
        "<leader>y" = {
          action = "\"+y";
          desc = "yank to clipboard";
        };
        "<leader>Y" = {
          action = "\"+y$";
          desc = "yank (untill line end) to clipboard";
        };

        # Stay in the middle
        "<c-d>" = {
          action = "<c-d>zz";
          desc = "half-page jump down";
        };
        "<c-u>" = {
          action = "<c-u>zz";
          desc = "half-page jump up";
        };
        "n" = {
          action = "nzzzv";
          desc = "next search result";
        };
        "N" = {
          action = "Nzzzv";
          desc = "prev search result";
        };
      };
      visual = {
        # Moving Selections
        "J" = {
          action = ":m '>+1<cr>gv=gv";
          desc = "move selection down";
        };
        "K" = {
          action = ":m '<-2<CR>gv=gv";
          desc = "move selection up";
        };

        # System Clipboard
        "<leader>d" = {
          action = "\"+d";
          desc = "delete to clipboard";
        };
        "<leader>p" = {
          action = "\"+p";
          desc = "paste from clipboard";
        };
        "<leader>P" = {
          action = "\"+P";
          desc = "paste from clipboard";
        };
        "<leader>y" = {
          action = "\"+y";
          desc = "yank to clipboard";
        };

        # Misc
        "<leader><leader>p" = {
          action = "\"_dP";
          desc = "paste and keep content";
        };
      };
    };
  };
}
