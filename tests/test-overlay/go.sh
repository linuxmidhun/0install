#!/bin/sh
if [ "$1" = "" ]; then
  echo Error
  exit 1
fi
exec cat /usr/games/plash-test/fixed.txt > "$1"
