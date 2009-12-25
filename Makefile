.PHONY: all docs test clean

all:
	erlc -o ebin src/*.erl

docs:
	erl -noshell \
	    -eval "edoc:application(minipush, \".\", [{dir, \"doc\"}, {packages, false}])" \
	    -s init stop

test:
	erl -noshell -pa ebin -sname test \
	    -eval "eunit:test({dir, \"ebin\"}, [verbose])" \
	    -s init stop

clean:
	rm -f ebin/*.beam
	rm -f src/*.beam
	rm -f erl_crash.dump
	rm -r -f doc/*
	touch doc/.EMPTY
	touch ebin/.EMPTY