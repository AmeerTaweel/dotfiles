{...}: {
  programs.nixvim = {
    options = {
      # Hybrid Line Numbers
      relativenumber = true; # Relative line numbers
      number = true; # Show current line number

      # Search
      incsearch = true; # Select items found in search
      hlsearch = true; # Highlight searches by default
      ignorecase = true; # Ignore case when searching
      smartcase = true; # Unless you type a capital

      # Indentation
      breakindent = true; # Keep indentation for wrapped lines
      autoindent = true; # Enable auto-indentation
      smartindent = true; # Enable smart-indentation
      expandtab = false; # Use tabs not spaces
      shiftwidth = 4; # Set tab width
      tabstop = 4;
      softtabstop = 0; # Use hard tabs always

      # Temporary Files
      undofile = true; # Enable persistent undo
      backup = true; # Enable backup files
      swapfile = true; # Enable swap files

      # Other
      textwidth = 80; # Set chars-per-line-limit
      colorcolumn = "81"; # Show column indicating the chars-per-line limit
      cursorline = true;
      cursorcolumn = true;
      termguicolors = true; # Enable true color
      wrap = false; # Do not wrap lines
      laststatus = 2; # Always show status line for all windows
      confirm = true; # Confirm operations that would fail normally (like closing an unsaved buffer)
      backspace = "indent,eol,start"; # Allow backspacing over indention, line breaks and insertion start
      autoread = true; # Automatically re-read files if modified outside the editor
      hidden = true; # Allow leaving buffers without saving, leaving buffer in background
      mouse = "a"; # Enable mouse support for scrolling and resizing
      title = true; # Set the window’s title, reflecting the file currently being edited
      wildmenu = true; # Enable expanding commands with the tab key
      splitright = true;
      splitbelow = true;
      history = 10000; # Set the number of saved commands in history
      showcmd = true; # Show incomplete commands at the bottom
      modeline = false; # Ignore file’s mode lines
      signcolumn = "yes"; # Always show sign column
      guicursor = "i:block"; # Use a block cursor in insert mode
      completeopt = "menuone,noinsert,noselect,preview"; # Auto-completion options
      cmdheight = 1; # Give more space for displaying messages.
      updatetime = 100; # Long updatetime leads to noticeable delays
      timeoutlen = 500; # Time to wait for a keybinding to complete
      scrolloff = 10; # Number of vertical context lines
      sidescrolloff = 5; # Number of horizontal context lines
      list = true;
      listchars = {
        eol = "↲";
        trail = "·";
        space = ".";
        tab = "-->";
      };
      viewoptions = ["folds" "cursor"];
    };
    extraConfigLua = ''
      -- Don't clutter working directory with backup files
      vim.opt.backupdir:remove { "." }
      -- Use diff mode vertical split
      vim.opt.diffopt:append { "vertical" }
      -- Don't pass messages to |insertion-completion-menu|
      vim.opt.shortmess:append "c"
    '';
    globals = {
      netrw_banner = 0; # Remove the Netrw banner
    };
  };
}
