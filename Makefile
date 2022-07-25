# Install dependencies needed during development.
.PHONY : dev-deps
dev-deps :
	opam install . --deps-only --yes

.PHONY : test-parser
test-parser:
	dune exec ./test/parser/es_parser_test.exe

.PHONY : lock
lock:
	opam lock .
