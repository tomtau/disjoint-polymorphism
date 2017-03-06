all: build run

run:
	@stack exec disjoint-polymorphism-exe

build:
	@stack build

install:
	-make -C data
	@stack install

test:
	@stack test

clean:
	@stack clean

doc:
	@make -C doc

.PHONY: all build test run clean doc install
