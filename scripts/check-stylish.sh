#!/usr/bin/env bash

set -euo pipefail
export LC_ALL=C.UTF-8

[[ -x '/usr/bin/fd' ]] && FD="fd" ||  FD="fdfind"

$FD . './io-sim' -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell.yaml -i
$FD . './io-classes' -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell.yaml -i
