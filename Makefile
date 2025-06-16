
all: prover-dpll


Prover.o: Prover.hs Context.o
	ghc -package attoparsec -c Prover.hs

Context.o: Context.hs Formulas.o
	ghc -package attoparsec -c Context.hs

Formulas.o: Formulas.hs
	ghc -package attoparsec -c Formulas.hs

prover-dpll: Prover.o Context.o Formulas.o Main.hs 
	ghc -package sqlite-simple -package attoparsec-0.13.2.5 -package timeit Main.hs -o prover-dpll

clean:
	-rm *.hi *.o
	-rm prover-dpll
