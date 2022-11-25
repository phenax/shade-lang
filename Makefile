
hpack:
	hpack

run-test: hpack
	cabal v2-test

testw:
	nodemon -e .hs,.cabal --exec 'make hpack run-test || true'

