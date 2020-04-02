#!bin/bash

for f in tests/print/*.imp; do  # or wget-*.sh instead of *.sh
  imp "$f"
done
