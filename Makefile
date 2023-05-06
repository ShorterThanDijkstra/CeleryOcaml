build:
	dune build

clean:
	dune clean

tests:
	dune runtest ./test

run:
	dune exec ./src/eval/repl.exe
