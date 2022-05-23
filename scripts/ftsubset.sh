#!/bin/sh

in="$1"
outstem="$2"
translations="$3"

for tr in "$translations"/*.json; do
	lang="${tr#"$translations"/}"
	lang="${lang%.json}"
	pyftsubset "$in" --text="$(jq .title "$tr")" --layout-features='' --flavor=woff2 \
		--output-file="$outstem.$lang.woff2"
done
