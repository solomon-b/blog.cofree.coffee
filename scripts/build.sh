#!/usr/bin/env bash

#######################
### Build New Posts ###
#######################

cd org
for i in *.org ; do
    NAME=${i%.*}
    echo "$NAME"

    if [[ ! -f ../markdown/$NAME.md ]]; then
      pandoc -s $i -t markdown -o ../markdown/$NAME.md
    fi
     
    if [[ ! -d ../html/$NAME ]]; then
      mkdir ../html/$NAME
      pandoc --template=../scripts/template.html -s $i -t html -o ../html/$NAME/index.html ;
    fi
done

cd ..

####################
### Create Index ###
####################

# cd into dir or die
cd html || exit 2;

# touch index.html and check if writable or die
touch "index.html"&& test -w "index.html" || exit 2;

# find folders
POSTS=$(find -maxdepth 1 -type d -not -path . -printf '%f\n' )
HEADER=$(<../scripts/header.html)
FOOTER=$(<../scripts/footer.html)
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
