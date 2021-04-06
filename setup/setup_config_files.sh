#!/bin/bash

: '
This is the main configuration setup script.

It will go through the individual directories of the `src` directory, and link them to the suitable place.

It determines the suitable place based on the contents of the `.setup` file in each directory.

The format of each line in the `.setup` file is:
'SELECTOR' : 'SELECT_TYPE' : 'DIST_DIRECTORY'

The selector is a file/directory selector that can be passed to the `find` command.

The select_type is either f or d, to determine to what to apply the selector (files or directories).

The dist_directory is the place where all files/directories the `find` matches with the selector
will be linked to.
'

# Constants
tab="    "
## ANSI color code variables
black="\e[0;30m"
red="\e[0;31m"
green="\e[0;32m"
yellow="\e[0;33m"
blue="\e[0;34m"
purple="\e[0;35m"
cyan="\e[0;36m"
white="\e[0;37m"
bold="\e[1m"
uline="\e[4m"
reset="\e[0m"

echo
echo "Configuration files setup started."
echo

# Important directory references
root_dir=$(dirname "$(pwd)")
src_dir=$root_dir/src
setup_dir=$root_dir/setup

# Print directory info
# Useful for debugging
echo -e $yellow $bold
echo "+-------------------+"
echo "| Base directories: |"
echo "+-------------------+"
echo -e $reset
echo
echo -e "${blue}${bold}Root directory:${reset} $root_dir"
echo -e "${blue}${bold}src directory:${reset} $src_dir"
echo -e "${blue}${bold}setup directory:${reset} $setup_dir"
echo

# Make sure the src and setup directories exist
if [[ ! -d $src_dir ]]; then
	echo -e "${red}${bold}ERROR: The src directory was not found in project root, terminating setup...${reset}"
	echo
	exit 1
fi
if [[ ! -d $setup_dir ]]; then
	echo -e "${red}${bold}ERROR: The setup directory was not found in project root, terminating setup...${reset}"
	echo
	exit 1
fi

# Get a list of all files and directories under `src`, except for . and ..
src_entries=$(ls $src_dir -A)

echo -e $yellow $bold
echo "+----------------------------------------+"
echo "| Iterating through the src directory... |"
echo "+----------------------------------------+"
echo -e $reset
echo

setup_line_regex="^'(.+)' : '(d|f)' : '(.+)'$"
for entry in $src_entries; do
	# For each directory run the script recursively
	if [[ -d $src_dir/$entry ]]; then
		echo -e "* Directory ${yellow}${bold}$entry${reset} is directly under ${yellow}${bold}src${reset}. Searching for ${cyan}${bold}.setup${reset} file..."

		if [[ -f $src_dir/$entry/.setup ]]; then
			echo -e "${tab}${cyan}${bold}.setup${reset} file found. Parsing..."

			# Parse line by line
			while read line; do
				# If line matches regex, continue linking
				if [[ $line =~ $setup_line_regex ]]; then

					# Extract variables from line
					selector=${BASH_REMATCH[1]}
					select_type=${BASH_REMATCH[2]}
					dist_dir="${BASH_REMATCH[3]/#\~/$HOME}"

					# Create dist_dir if it does not exist
					if [[ ! -d $dist_dir ]]; then
						mkdir -p "$dist_dir"
					fi

					# Find all files/directories matching the selector and select_type.
					# More about this type of loop at: https://stackoverflow.com/a/9612114/6835329
					find $src_dir/$entry -type $select_type -wholename "$selector" -not -name ".setup" -print0 | while read -d $'\0' sub_entry_path
					do
						# Now we need to link each file/directory to the dist_directory
						sub_entry_name=$(basename $sub_entry_path)
						if [[ -d $sub_entry_path ]]; then
							echo -e "${tab}Linking ${yellow}${bold}$sub_entry_name${reset} to ${yellow}${bold}$dist_dir${reset}"
							ln -sf "$sub_entry_path" "$dist_dir"
						else
							echo -e "${tab}Linking ${cyan}${bold}$sub_entry_name${reset} to ${yellow}${bold}$dist_dir${reset}"
							ln -sf "$sub_entry_path" "$dist_dir/$sub_entry_name"
						fi
					done
				else
					echo -e "${tab}${red}${bold}ERROR: Invalid '.setup' file, terminating setup...${reset}"
					echo
					exit 1
				fi

			done < $src_dir/$entry/.setup

			echo -e "${tab}${green}${bold}Linking successful.${reset}"

		else
			echo -e "${tab}The ${cyan}${bold}.setup${reset} file was not found under directory ${yellow}${bold}$entry${reset}. It will be ignored."
		fi

		echo
	else
		# Skip files directly under src
		echo -e "* File ${cyan}${bold}$entry${reset} is directly under ${yellow}${bold}src${reset}. It will be ignored."
		echo
	fi
done

echo -e "${green}${bold}Configuration files setup completed successfully.${reset}"
echo
