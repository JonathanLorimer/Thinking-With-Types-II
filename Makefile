ghcid:
	hpack .
	cabal new-build
	ghcid -o ghcid.txt --command 'cabal new-repl lib:ThinkingWithTypes' --warnings

format:
	ormolu --mode inplace $(find src/ -name '*.hs')
