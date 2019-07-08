
.PHONY: cabal-build
cabal-build:
	cabal new-build

.PHONY: stack-build
stack-build:
	stack build

.PHONY: weeder
weeder:
	weeder . --build

.PHONY: format
format:
	find src -name '*.hs' -exec echo "Formatting '{}'" \; -exec ormolu --mode=inplace '{}' \;

.PHONY: watch
watch:
	ghcid

.PHONY: vim
vim:
	echo ":e package.yaml\n:vsplit\n:e src/Unpuzzle/Games/Sokoban.hs\n:vsplit\n:Ghcid\n:term" | nvim -s -
