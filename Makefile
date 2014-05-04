PROGRAM_NAME = monadfun

all: $(PROGRAM_NAME)

$(PROGRAM_NAME): $(PROGRAM_NAME).hs
	ghc $(PROGRAM_NAME).hs

test: ${PROGRAM_NAME}
	cool --type ${f}.cl
	cool ${f}.cl > output.txt || echo "fail"
	./monadfun ${f}.cl-type > myout.txt
	diff -b -B -E -w output.txt myout.txt || echo "yay"
	rm output.txt myout.txt

run: ${PROGRAM_NAME}
	cool --type ${f}.cl
	./monadfun ${f}.cl-type

build:
	cp monadfun.hs submission/main.hs
	zip -r pa5.zip submission

clean:
	rm *.out
