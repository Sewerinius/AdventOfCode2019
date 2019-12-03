CC = ghc --make

all: A B

A: A.hs
	$(CC) -o A A.hs

B: B.hs
	$(CC) -o B B.hs

runall: runa runb

runa: A
	./A in.txt

runb: B
	./B in.txt

clean:
	rm A B A.hi B.hi A.o B.o