#!/bin/bash

: '
This script creates symbolic links in the $HOME/bin for the scripts in the supplied path.

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

# If no bin directory exists in the home directory, create one
if [[ ! -d ~/bin ]]; then
	mkdir ~/bin
fi

SHELL_SCRIPT_FILES_AND_DIRECTORIES=$(ls $WORKING_DIRECTORY)

SHELL_SCRIPT_FILE_REGEX="^.+\.sh$"
for ENTRY in $SHELL_SCRIPT_FILES_AND_DIRECTORIES; do
	# For each directory run the script recursively
	if [[ -d "${WORKING_DIRECTORY}/${ENTRY}" ]]; then
		echo -e "\nSetting up directory: $ENTRY\n"
		# Running the script with bash so it runs even if it's not executable
		bash $0 "${WORKING_DIRECTORY}/${ENTRY}"
		echo -e "\nDirectory $ENTRY setup complete.\n"
	elif [[ -f "${WORKING_DIRECTORY}/${ENTRY}" && $ENTRY =~ $SHELL_SCRIPT_FILE_REGEX ]]; then
		echo "Linking: $ENTRY"
		# Remove the .sh from the end of the link name in ~/bin
		ln -sf "${WORKING_DIRECTORY}/${ENTRY}" "${HOME}/bin/${ENTRY%.sh}"
	else
		echo "File $ENTRY will not be linked."
	fi
done
