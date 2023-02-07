#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

usage() {
	echo "Usage: $0 [-l <positive int>] [-p <string>]" 1>&2;
	exit 1;
}

# Parse Options
while getopts ":l:p:i" o; do
	case "${o}" in
		l)
			l=${OPTARG}
			((l > 0)) || usage
			;;
		p)
			p=${OPTARG}
			;;
		i)
			i=1
			;;
		*)
			usage
			;;
	esac
done
shift $((OPTIND-1))

cmd="rofi -sep ' ' -dmenu"

if   [[ -n "${l:-}" ]]; then
	cmd+=" -l ${l}"
fi

if [[ -n "${p:-}" ]]; then
	cmd+=" -p ${p}"
fi

if [[ ${i:-0} == 1 ]]; then
	cmd+=" -i"
fi

eval "${cmd}"
