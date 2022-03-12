local paths = require "plugins.lsp.paths"

local customize_server_options = function(config)
	config.cmd = {
		paths.csharp,
		"--languageserver",
		"--hostPID",
		tostring(vim.fn.getpid())
	};
end

return customize_server_options
