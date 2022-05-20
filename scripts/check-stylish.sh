#!/usr/bin/env bash

set -euo pipefail
export LC_ALL=C.UTF-8

fd . './io-sim' -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell.yaml -i
fd . './io-classes' -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell.yaml -i
fd . './strict-stm' -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell.yaml -i
