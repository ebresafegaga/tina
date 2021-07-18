

automaton: 
	menhir grammar.mly --dump

messages: grammar.mly
	menhir --list-errors grammar.mly > messages

conflicts: 
	menhir grammar.mly --explain

clean: 
	rm grammar.ml grammar.mli grammar.automaton grammar.conflicts 
