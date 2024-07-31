# haskell-minisat
An Implementation of MiniSAT in Haskell.
Follows the minisat paper here ([MiniSAT](http://minisat.se/downloads/MiniSat.pdf)). 
More info [MiniSAT](http://minisat.se/MiniSat.html).

## Build

Haskell-MiniSAT is built using Cabal (currently v3.10.2.1)

- Clone the repo

```
git clone https://github.com/jchilds0/haskell-minisat
cd haskell-minisat
```

- Build and Test

```
cabal build
cabal test
```

- Extract benchmark files and run benchmark (optional). `src/Bench/Main.hs` should be modified depending on the number of benchmarks to run (recommended)

```
tar -xvf benchmarks.tar.gz
cabal bench
```


