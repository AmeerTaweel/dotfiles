local M = {}

M.peek_definition = function()
	local params = vim.lsp.util.make_position_params()
	return vim.lsp.buf_request(0, "textDocument/definition", params, function(_, result)
		if result == nil or vim.tbl_isempty(result) then
			print("Can't peek definition.")
	 	 	return
	 	end
	 	vim.lsp.util.preview_location(result[1])
	end)
end

return M
