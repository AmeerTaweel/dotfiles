--[[
+----------------------+
| # LSP Configurations |
+----------------------+
--]]

require "globals"
local lspConfig = require "lspconfig"
local lspInstall = require "nvim-lsp-installer"
local whichKey = require "which-key"
local vimEventSystem = require "vim-event-system"
local utils = require "utils"

-- List of servers that will always be installed
-- TODO: Consider adding Deno LSP
local requiredServers = {
	"bashls", -- Bash
	"clangd", -- C++
	"diagnosticls", -- Diagnostics
	"jdtls", -- Java
	"kotlin_language_server", -- Kotlin
	"html", -- HTML
	"cssls", -- CSS
	"tsserver", -- TypeScript
	"svelte", -- Svelte
	"jsonls", -- JSON
	"pyright", -- Python
	"yamlls", -- YAML
	"sumneko_lua", -- Lua
	"vimls", -- VimScript
	"texlab" -- LaTex
}

local customSettings = {
	sumneko_lua = require "plugin-config.lsp.lua-language-server-config",
	kotlin_language_server = require "plugin-config.lsp.kotlin-language-server-config",
	diagnosticls = require "plugin-config.lsp.diagnostic-language-server-config"
}

-- Auto install servers
local autoInstallServers = function()
	for _, serverName in pairs(requiredServers) do
		-- Check that the server is supported in nvim-lsp-installer
		local isSupported, server = lspInstall.get_server(serverName)
		if isSupported then
			if not server:is_installed() then
				print("LSP Auto Install: Started installing " .. serverName .. ".")
				server:install()
			end
		else
			print("LSP Auto Install: " .. serverName .. " is unsupported.")
		end
	end
end

local configureDiagnostices = function(_, _, params, client, _)
	-- Set diagnostics options
	local diagnosticsOptions = {
		underline = true,
		virtual_text = false,
		signs = true,
		update_in_insert = false,
	}

	-- Show source in diagnostics
	local uri = params.uri
	local bufferNumber = vim.uri_to_bufnr(uri)
	local diagnostics = params.diagnostics

	if not bufferNumber then
		return
	end

	for i, v in ipairs(diagnostics) do
		diagnostics[i].message = string.format("%s: %s", v.source, v.message)
	end

	vim.lsp.diagnostic.save(diagnostics, bufferNumber, client)

	if not vim.api.nvim_buf_is_loaded(bufferNumber) then
		return
	end

	vim.lsp.diagnostic.display(diagnostics, bufferNumber, client, diagnosticsOptions)
end

local peekDefinition = function()
	local previewLocationCallback = function(_, _, result)
		if result == nil or vim.tbl_isempty(result) then
			return nil
		end
		vim.lsp.util.preview_location(result[1])
	end
	local params = vim.lsp.util.make_position_params()
	return vim.lsp.buf_request(0, "textDocument/definition", params, previewLocationCallback)
end

local onAttatch = function(client, bufferNumber)
    -- Enable 'omnifunc' compatible LSP completion
	vim.api.nvim_buf_set_option(bufferNumber, "omnifunc", "v:lua.vim.lsp.omnifunc")

	-- Diagnostics custom configuration
	vim.lsp.handlers["textDocument/publishDiagnostics"] = configureDiagnostices

	-- Map buffer-local keybindings when the language server attaches
	whichKey.register({
		g = {
			D = { vim.lsp.buf.declaration, "GoTo Declaration" },
			d = { vim.lsp.buf.definition, "GoTo Definition" },
			i = { vim.lsp.buf.implementation, "GoTo Implementation" },
			r = { vim.lsp.buf.references, "GoTo References <Quickfix List>" },
			x = { peekDefinition, "Peek Definition" }
		},
		["[d"] = { vim.lsp.diagnostic.goto_prev, "Previous Diagnostic" },
		["]d"] = { vim.lsp.diagnostic.goto_next, "Next Diagnostic" },
		["<c-k>"] = { vim.lsp.buf.signature_help, "Signature Help" },
		K = {vim.lsp.buf.hover, "Hover" }
	}, {buffer = bufferNumber})
	whichKey.register({
		rn = { vim.lsp.buf.rename, "Rename Symbol" },
		ac = { vim.lsp.buf.code_action, "Code Actions" },
		D = { vim.lsp.buf.type_definition, "GoTo Type Definition" }
	}, { prefix = key.leader, buffer = bufferNumber })

	-- TODO: Fix theses
	-- vim.api.nvim_buf_set_keymap(bufnr, 'v', '<leader>ca', '<cmd>lua vim.lsp.buf.range_code_action()<CR>', opts)
	-- vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>so', [[<cmd>lua require('telescope.builtin').lsp_document_symbols()<CR>]], opts)

	vimEventSystem.createCustomEditorCommand("LspFormat", vim.lsp.buf.formatting)
	vimEventSystem.createCustomEditorCommand("LspListDiagnostics", vim.lsp.diagnostic.set_loclist)
	utils.createAutoCommand "CursorHold,CursorHoldI * lua vim.lsp.diagnostic.show_line_diagnostics({focusable=false})"

	-- Set highlighting autocommand if server supports that
	if client.resolved_capabilities.document_highlight then
		utils.createAutoGroup("LSPDocumentHighlight", {
			"CursorHold <buffer> lua vim.lsp.buf.document_highlight()",
			"CursorMoved <buffer> lua vim.lsp.buf.clear_references()"
		})
	end
end

local getDefaultServerConfig = function()
	local capabilities = vim.lsp.protocol.make_client_capabilities()
	-- Enable snippet support
	capabilities.textDocument.completion.completionItem.snippetSupport = true

	return {
		capabilities = capabilities,
		on_attach = onAttatch
	}
end

local setupServers = function(server)
	-- Loads installed servers, and sets them up
	-- local servers = lspInstall.installed_servers()
	-- for _, server in pairs(servers) do
		local config = getDefaultServerConfig()

		if customSettings[server] ~= nil then
			-- Apply custom settings, if found
			for key, value in pairs(customSettings[server]) do
				config[key] = value
			end
		end

		-- lspConfig[server].setup(config)
		server:setup(config)
	-- end
end

lspInstall.on_server_ready(function(server)
    local opts = {}

    -- (optional) Customize the options passed to the server
    -- if server.name == "tsserver" then
    --     opts.root_dir = function() ... end
    -- end

    -- This setup() function is exactly the same as lspconfig's setup function (:help lspconfig-quickstart)
	setupServers(server)
    -- server:setup(opts)
    vim.cmd [[ do User LspAttachBuffers ]]
	-- TODO: fix theme manager
	-- vim.cmd "colorscheme ayu"
end)

-- Setup auto language server installer
-- lspInstall.setup()

-- Install missing servers, if any
autoInstallServers()

-- Initial server setup
-- setupServers()
