#!/bin/sh
# usage: tools/samples_diff.sh  -- compare every sample with ezdxf, print the distinct differences (needs `cabal build exe:dxf-summary`)
S=$(cabal list-bin dxf-summary)
for f in samples/*.dxf; do
  case "$f" in *"3CG"*) continue;; esac
  $S "$f" > /tmp/sd.json 2>/tmp/sd.err || { echo "PARSE-FAIL $f $(head -c 200 /tmp/sd.err)"; continue; }
  .ref/venv/bin/python -I tools/compare.py "$f" /tmp/sd.json | grep -E '^(DIFF|MISSING)' | sed -e "s|^|$(basename "$f" .dxf | cut -c1-24): |"
done
