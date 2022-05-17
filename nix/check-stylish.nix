{ runCommand, fd, lib, stylish-haskell }:

runCommand "check-stylish" {
  meta.platforms = with lib.platforms; [ linux ];
  buildInputs = [ fd stylish-haskell ];
  src = ./..;
} ''
  unpackPhase
  cd $sourceRoot
  bash ./scripts/check-stylish.sh
  diff -ru $src .

  EXIT_CODE=$?
  if [[ $EXIT_CODE != 0 ]]
  then
    diff -ru $src .
    echo "*** Stylish-haskell found changes that need addressed first"
    exit $EXIT_CODE
  else
    echo $EXIT_CODE > $out
  fi
''
