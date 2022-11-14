
run-test:
	cabal v2-test

testw:
	nodemon -e .hs --exec 'make run-test || true'

