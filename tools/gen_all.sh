#!/bin/sh
# The fixtures of the test suite (test/data/*.dxf), written by ezdxf: they are generated, not committed.
#   tools/gen_all.sh            # needs .ref/venv (python -m venv .ref/venv && .ref/venv/bin/pip install ezdxf)
set -e
cd "$(dirname "$0")/.."
mkdir -p test/data
for g in tools/gen_*.py; do
  echo "$g"
  .ref/venv/bin/python -I "$g"
done
