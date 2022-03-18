require "globals"
local lsp_config = require "lspconfig"
local vim_utils = require "utils.vim"
local cmp = require "cmp_nvim_lsp" -- auto completion capabilities

-- NOTE: Both Java and Kotlin language servers do not support single files.
local servers = {
	"bashls", -- Bash
	"ccls", -- C/C++
	"html", -- HTML
	"cssls", -- CSS
	"tsserver", -- JavaScript/TypeScript
	"jsonls", -- JSON
	"pyright", -- Python
	"yamlls", -- YAML
	"vimls", -- VimScript
	"sumneko_lua", -- Lua
	"diagnosticls", -- Diagnostics
	"rust_analyzer", -- Rust
	"rnix" -- Nix
}

local customize_server_options = {
	diagnosticls = require "plugins.lsp.diagnostic",
}

local get_diagnostics_options = function()
	local options = {
		underline = true,
		virtual_text = false,
		signs = true,
		update_in_insert = false
	}

	return vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, options)
end

local on_attach = function(client, buffer_num)
    -- Enable 'omnifunc' compatible LSP completion
	vim.api.nvim_buf_set_option(buffer_num, "omnifunc", "v:lua.vim.lsp.omnifunc")

	-- Diagnostics custom configuration
	vim.lsp.handlers["textDocument/publishDiagnostics"] = get_diagnostics_options()

	vim_utils.create_auto_command "CursorHold,CursorHoldI * lua vim.diagnostic.open_float({ focusable = false })"

	-- Set highlighting autocommand if server supports that
	if client.resolved_capabilities.document_highlight then
		vim_utils.create_auto_group("LSPDocumentHighlight", {
			"CursorHold <buffer> lua vim.lsp.buf.document_highlight()",
			"CursorMoved <buffer> lua vim.lsp.buf.clear_references()"
		})
	end
end

local get_default_server_config = function()
	local capabilities = vim.lsp.protocol.make_client_capabilities()
	-- Enable snippet support
	capabilities.textDocument.completion.completionItem.snippetSupport = true

	-- nvim-cmp supports more LSP capabilities
	capabilities = cmp.update_capabilities(capabilities)

	return {
		capabilities = capabilities,
		on_attach = on_attach
	}
end

local setup_servers = function()
	for _, server in ipairs(servers) do
		local config = get_default_server_config()

		if customize_server_options[server] then
			-- Apply the custom options to the default ones
			customize_server_options[server](config)
		end

		lsp_config[server].setup(config)
	end
end

setup_servers()
