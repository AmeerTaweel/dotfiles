{...}: {
  programs.nixvim = {
    keymaps = [
      # Window Splits
      {
        mode = "n";
        key = "<leader>-";
        action = "<cmd>split<cr>";
        options.desc = "split window horizontally";
      }
      {
        mode = "n";
        key = "<leader>/";
        action = "<cmd>vsplit<cr>";
        options.desc = "split window vertically";
      }

      # Tabs
      {
        mode = "n";
        key = "<leader>tc";
        action = "<cmd>tabclose<cr>";
        options.desc = "close tab";
      }
      {
        mode = "n";
        key = "<leader>tn";
        action = "<cmd>tabnew<cr>";
        options.desc = "new tab";
      }

      # Misc
      {
        mode = "n";
        key = "<cr>";
        action = "o<esc>";
        options.desc = "insert new line";
      }
      {
        mode = "n";
        key = "<space>";
        action = ",";
        options.desc = "repeat last f/t/F/T in opposite direction";
      }
      {
        mode = "n";
        key = "<leader>th";
        action = "<cmd>noh<cr>";
        options.desc = "remove last search highlights";
      }

      # System Clipboard
      {
        mode = "n";
        key = "<leader>d";
        action = "\"+d";
        options.desc = "delete to clipboard";
      }
      {
        mode = "n";
        key = "<leader>D";
        action = "\"+d$";
        options.desc = "delete (untill line end) to clipboard";
      }
      {
        mode = "n";
        key = "<leader>p";
        action = "\"+p";
        options.desc = "paste from clipboard";
      }
      {
        mode = "n";
        key = "<leader>P";
        action = "\"+P";
        options.desc = "paste from clipboard";
      }
      {
        mode = "n";
        key = "<leader>y";
        action = "\"+y";
        options.desc = "yank to clipboard";
      }
      {
        mode = "n";
        key = "<leader>Y";
        action = "\"+y$";
        options.desc = "yank (untill line end) to clipboard";
      }

      # Stay in the middle
      {
        mode = "n";
        key = "<c-d>";
        action = "<c-d>zz";
        options.desc = "half-page jump down";
      }
      {
        mode = "n";
        key = "<c-u>";
        action = "<c-u>zz";
        options.desc = "half-page jump up";
      }
      {
        mode = "n";
        key = "n";
        action = "nzzzv";
        options.desc = "next search result";
      }
      {
        mode = "n";
        key = "N";
        action = "Nzzzv";
        options.desc = "prev search result";
      }

      # Moving Selections
      {
        mode = "v";
        key = "J";
        action = ":m '>+1<cr>gv=gv";
        options.desc = "move selection down";
      }
      {
        mode = "v";
        key = "K";
        action = ":m '<-2<CR>gv=gv";
        options.desc = "move selection up";
      }

      # System Clipboard
      {
        mode = "v";
        key = "<leader>d";
        action = "\"+d";
        options.desc = "delete to clipboard";
      }
      {
        mode = "v";
        key = "<leader>p";
        action = "\"+p";
        options.desc = "paste from clipboard";
      }
      {
        mode = "v";
        key = "<leader>P";
        action = "\"+P";
        options.desc = "paste from clipboard";
      }
      {
        mode = "v";
        key = "<leader>y";
        action = "\"+y";
        options.desc = "yank to clipboard";
      }

      # Misc
      {
        mode = "v";
        key = "<leader><leader>p";
        action = "\"_dp";
        options.desc = "paste and keep content";
      }
      {
        mode = "v";
        key = "<leader><leader>P";
        action = "\"_dP";
        options.desc = "paste and keep content";
      }
    ];
  };
}
