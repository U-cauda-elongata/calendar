#!/bin/sh

set -e

elm_home="${ELM_HOME:-$HOME/.elm}/$(jq -r '."elm-version"' elm.json)"

jq -r '.dependencies | values | .[] | to_entries | .[] | [.key, .value] | @tsv' elm.json \
| while read -r pkg ver; do
	echo "## $pkg $ver"
	echo
	# Print line-by-line to unify final-newline behaviors.
	while read -r line; do
		echo "$line"
	done < "$elm_home/packages/$pkg/$ver/LICENSE"
	echo
done
