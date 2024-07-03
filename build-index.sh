#!/usr/bin/env bash

# Build Index

# cd into dir or die
cd html || exit 2;

# touch index.html and check if writable or die
touch "index.html"&& test -w "index.html" || exit 2;

# find folders
POSTS=$(find -maxdepth 1 -type d -not -path . -printf '%f\n' | sort -r)
HEADER=$(<../html-template/header.html)
FOOTER=$(<../html-template/footer.html)
(
  echo "$HEADER"
  for POSTDIR in $POSTS ; do
# Capitalize and replace '-' with ' '
POST=$(sed -E 's/[[:alpha:]]+/\u&/g;s/-/\ /g' <<< "${POSTDIR:11}" )  
echo "    <li><a href=\"${POSTDIR}\">${POST}</li>"
  done
  echo "$FOOTER"
) > "index.html"

exit
