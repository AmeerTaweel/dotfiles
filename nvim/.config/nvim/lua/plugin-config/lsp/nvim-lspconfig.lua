require "globals"
local lsp_config = require "lspconfig"
local utils = require "utils"

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
	"texlab", -- LaTex
	"diagnosticls", -- Diagnostics
}

local customize_server_options = {
	diagnosticls = require "plugin-config.lsp.diagnostic-language-server"
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

	utils.createAutoCommand "CursorHold,CursorHoldI * lua vim.diagnostic.open_float({ focusable = false })"

	-- Set highlighting autocommand if server supports that
	if client.resolved_capabilities.document_highlight then
		utils.createAutoGroup("LSPDocumentHighlight", {
			"CursorHold <buffer> lua vim.lsp.buf.document_highlight()",
			"CursorMoved <buffer> lua vim.lsp.buf.clear_references()"
		})
	end
end

local get_default_server_config = function()
	local capabilities = vim.lsp.protocol.make_client_capabilities()
	-- Enable snippet support
	capabilities.textDocument.completion.completionItem.snippetSupport = true

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
