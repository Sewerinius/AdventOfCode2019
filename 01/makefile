CC = ghc --make

all: a b

a: a.hs
	$(CC) -o a a.hs

b: b.hs
	$(CC) -o b b.hs

run: a b
	./a in.txt
	./b in.txt

clean:
	rm a b a.hi b.hi a.o b.o