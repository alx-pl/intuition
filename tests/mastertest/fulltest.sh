#!/bin/bash

#ALLES=100
ALLES=239097

./prover-dpll -d 1 $ALLES ./formulas.db
./prover-vanilla -d 1 $ALLES ./formulas.db
./prover-dpll -d 1 $ALLES ./formulas.db
./prover-vanilla -d 1 $ALLES ./formulas.db

