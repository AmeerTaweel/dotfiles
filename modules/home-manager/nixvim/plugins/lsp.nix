{...}: {
  programs.nixvim.plugins.lsp = {
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
}
