PROGRAM_NAME = main

all: $(PROGRAM_NAME)

$(PROGRAM_NAME): $(PROGRAM_NAME).hs
	ghc $(PROGRAM_NAME).hs

test:
	zip -r TDD.zip testcases

clean:
	rm *.out
