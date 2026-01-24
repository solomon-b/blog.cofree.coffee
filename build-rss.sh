#!/usr/bin/env bash

# Build RSS Feed

# cd into dir or die
cd html || exit 2;

# touch feed.xml and check if writable or die
touch "feed.xml" && test -w "feed.xml" || exit 2;

# find folders (sorted newest first)
POSTS=$(find -maxdepth 1 -type d -not -path . -printf '%f\n' | sort -r)

(
  echo '<?xml version="1.0" encoding="UTF-8"?>'
  echo '<rss version="2.0">'
  echo '  <channel>'
  echo '    <title>Solomon'"'"'s Blog</title>'
  echo '    <link>https://blog.cofree.coffee</link>'
  echo '    <description>functional programming, permaculture, math</description>'

  for POSTDIR in $POSTS ; do
    # Extract title from the generated HTML file
    TITLE=$(grep -oP '(?<=<title>).*(?=</title>)' "${POSTDIR}/index.html")

    # Extract first <p> tag content as description (handles multi-line paragraphs)
    DESC=$(perl -0777 -ne 'if (/<p>(.*?)<\/p>/s) { $d = $1; $d =~ s/<[^>]*>//g; $d =~ s/\n/ /g; $d =~ s/\s+/ /g; print $d; }' "${POSTDIR}/index.html")

    # Extract date from directory name (YYYY-MM-DD prefix) and convert to RFC 822
    DATE_PREFIX=$(echo "$POSTDIR" | grep -oP '^\d{4}-\d{2}-\d{2}')
    PUBDATE=$(date -d "$DATE_PREFIX" -R 2>/dev/null || echo "")

    echo '    <item>'
    echo "      <title>${TITLE}</title>"
    echo "      <link>https://blog.cofree.coffee/${POSTDIR}</link>"
    echo "      <pubDate>${PUBDATE}</pubDate>"
    echo "      <description>${DESC}</description>"
    echo '    </item>'
  done

  echo '  </channel>'
  echo '</rss>'
) > "feed.xml"

exit
