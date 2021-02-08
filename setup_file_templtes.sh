#!/bin/bash

: '
Setup file templates
'

if [[ -d "templates" ]]; then
	echo "Found file templates."

	echo "Setting up files templates with vim..."
	WORKING_DIRECTORY=$(pwd)
	ln -sf "${WORKING_DIRECTORY}/templates" "${HOME}/.vim/templates"
	echo "VIM file templates setup complete."
else
	echo "Could not find file templates."
fi
