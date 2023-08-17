#!/usr/bin/env bash

# Optical Character Recognition

set -o errexit
set -o pipefail
set -o nounset

TMP=$(mktemp -d)
IMG="${TMP}/img"
TXT="${TMP}/txt"

maim --select > "${IMG}"

tesseract "${IMG}" "${TXT}"

# Tesseract adds ".txt" to the text filename
xclip -selection clipboard < "${TXT}.txt"

rm -rf "${TMP}"
