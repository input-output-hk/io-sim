#!/usr/bin/env bash

set -euo pipefail
export LC_ALL=C.UTF-8

[[ -x '/usr/bin/fd' ]] && FD="fd" ||  FD="fdfind"

$FD -E Setup.hs -g '*.hsc?$' io-sim -X stylish-haskell -c .stylish-haskell.yaml -i
$FD -E Setup.hs -E src/Control/Concurrent/Class/MonadSTM.hs -g '*.hsc?$' io-classes/src -X stylish-haskell -c .stylish-haskell.yaml -i
$FD -E Setup.hs -g '*.hsc?$' io-classes/strict-mvar -X stylish-haskell -c .stylish-haskell.yaml -i
$FD -E Setup.hs -g '*.hsc?$' io-classes/strict-stm -X stylish-haskell -c .stylish-haskell.yaml -i
$FD -E Setup.hs -g '*.hsc?$' io-classes/si-timers -X stylish-haskell -c .stylish-haskell.yaml -i
