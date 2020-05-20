# Project Euler in Haskell

## Boilerplate repository - solutions are not here!

Usage:
* `stack exec euler -- (-r|--run N)`

  Run solution for problem N. If you run it for the first time and the solution is correct, do not forget to update `txt/answers.txt` by adding output of the program to the Nth line of file.
* `stack exec euler -- (-t|--tests)`

  Generate tests for all problems.

  Warning: running all tests may take eternity. If you wish to run tests only for utils functions, delete all files in `test/Simulations`.
* `stack exec euler -- (-g|--generate N)`

  Generate template file `src/Solutions/EulerN.hs` for solution of problem N and imports in file `src/Main/Run/Eulers.hs`.
* `stack exec euler -- (-d|--delete N)`

  Reverse action done by `--generate`.
* `stack exec euler -- (-s|--simulate N)`

  Run simulation associated with problem N (creating such simulation is a manual process consisting of writing code in `src/Simulations/EulerN.hs` and adding import to `src/Main/Simulate.hs`).