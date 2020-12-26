#!/bin/bash

: '
This script creates symbolic links in the home directory
for the dotfiles in the supplied path.

If no path is supplied, uses the current working directory.
'

SCRIPT_PATH=$(pwd)

# Use the supplied path as the working directory
# If no path supplied, use the current working directory
if [[ $1 ]]; then
	# Make sure supplied path is a directory
	if [[ ! -d $1 ]]; then
		# Exit if supplied path is not a directory
		echo "$1 is not a directory."
		exit 1
	fi
	cd $1
	WORKING_DIRECTORY=$(pwd)
	cd $SCRIPT_PATH
else
	WORKING_DIRECTORY=$SCRIPT_PATH
fi

# List hidden files since dotfiles are hidden files, but don't list . and ..
DOT_FILES_AND_DIRECTORIES=$(ls $WORKING_DIRECTORY -A)

DOTFILE_REGEX="^\..+$"
for ENTRY in $DOT_FILES_AND_DIRECTORIES; do
	# For each directory run the script recursively
	if [[ -d "${WORKING_DIRECTORY}/${ENTRY}" ]]; then
		echo -e "\nSetting up directory: $ENTRY\n"
		# Running the script with bash so it runs even if it's not executable
		bash $0 $ENTRY
		echo -e "\nDirectory $ENTRY setup complete.\n"
	elif [[ -f "${WORKING_DIRECTORY}/${ENTRY}" && $ENTRY =~ $DOTFILE_REGEX ]]; then
		echo "Linking: $ENTRY"
		ln -sf "${WORKING_DIRECTORY}/${ENTRY}" "${HOME}/${ENTRY}"
	else
		echo "File $ENTRY will not be linked."
	fi
done
