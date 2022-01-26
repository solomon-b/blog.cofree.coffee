#!/usr/bin/env bash

# Build New Posts
cd org
for i in *.org ; do
    name=${i%.*}
    echo "$name"

    if [[ ! -f ../markdown/$name.md ]]; then
      pandoc -s $i -t markdown -o ../markdown/$name.md
    fi
     
    if [[ ! -d ../html/$name ]]; then
      mkdir ../html/$name
      pandoc -s $i -t html -o ../html/$name/index.html ;
    fi
done
