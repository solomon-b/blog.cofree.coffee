#!/usr/bin/env bash

# Runs each tangled GHCi session and compares the output with the post.
#
# A session file is 'Module.Name.ghci'. GHCi loads that module first. A line
# with a prompt is input. Every other line is the output that the post shows.

set -euo pipefail

PROMPT='^[[:space:]]*(λ>|ghci>|>) ?'
status=0

# Tangle puts an empty line between blocks. A call stack location depends on the
# build path and on the Alex version. Ignore both.
normalize() {
  sed -E -e 's/[[:space:]]*$//' -e '/^$/d' -e 's/, called at .*/, called at <location>/'
}

# check SESSION_FILE SOURCE_ROOT...
check() {
  local session=$1
  shift
  local module expected actual
  module=$(basename "$session" .ghci)
  expected=$(grep -Ev "$PROMPT" "$session" | normalize || true)
  actual=$(grep -E "$PROMPT" "$session" | sed -E "s/$PROMPT//" |
    ghci -v0 -ignore-dot-ghci "${@/#/-i}" "$module" 2>&1 | normalize || true)
  if [ "$expected" = "$actual" ]; then
    echo "ok   $session"
  else
    echo "FAIL $session"
    diff <(echo "$expected") <(echo "$actual") || true
    status=1
  fi
}

for session in tangled/sessions/*.ghci; do
  check "$session" tangled
done

# A grammar directory is its own source root.
for session in tangled/grammar/*/*.ghci; do
  check "$session" tangled "$(dirname "$session")"
done

exit $status
