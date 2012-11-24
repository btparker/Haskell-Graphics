SRC = Main.hs Vector3.hs

all: main

optimized: $(SRC)
	ghc -O2 --make -o main Main.hs

main: $(SRC)
	ghc -rtsopts -auto-all -prof -O2 --make -o main Main.hs

clean:
	rm -f main *.o *.hi