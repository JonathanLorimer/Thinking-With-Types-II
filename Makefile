ghcid:
	hpack .
	cabal new-build
	ghcid -o ghcid.txt --command 'cabal new-repl lib:ThinkingWithTypes' --allow-eval --warnings

format:
	ormolu --mode inplace $(find src/ -name '*.hs')
