{...}: {
  programs.nixvim = {
    plugins = {
      lsp = {
        enable = true;
        servers = {
          lua-ls.enable = true;
          pyright.enable = true;
          nil_ls.enable = true;
          html.enable = true;
          cssls.enable = true;
          jsonls.enable = true;
          yamlls.enable = true;
          ccls.enable = true;
        };
      };
      nvim-cmp = {
        enable = true;
        completion = {
          autocomplete = ["TextChanged"];
          keywordLength = 1;
        };
        sources = [
          {name = "nvim_lsp";}
          {name = "nvim_lua";}
          {name = "path";}
          {name = "buffer";}
        ];
        mapping = {
          "<tab>" = "cmp.mapping.select_next_item()";
          "<s-tab>" = "cmp.mapping.select_prev_item()";
          "<c-n>" = "cmp.mapping.select_next_item()";
          "<c-p>" = "cmp.mapping.select_prev_item()";
        };
      };
    };
  };
}
