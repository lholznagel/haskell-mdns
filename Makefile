.PHONY: test all

all: install test

install:
	stack install

test:
	stack test
