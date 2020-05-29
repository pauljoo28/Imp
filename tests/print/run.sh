#!bin/bash

for f in tests/print/*.imp; do  # or wget-*.sh instead of *.sh
  echo "$f"
  imp "$f"
done
