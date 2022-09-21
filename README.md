# expr-ssa
A program written in Haskell which compiles simple expressions to SSA (single
static assignment) programs.

Originally used to demonstrate GitHub Actions for undergraduates at University
of Kent 2021.

## Building and testing
This is a Haskell project built using Cabal, a package management tool for
Haskell. Assuming you have Cabal and a version of the GHC Haskell compiler
installed, running the following command in the base repository directory will
build the library:

    cabal build

To build and run tests (with nicer output):

    cabal test --test-show-details=streaming

Note that `cabal test` will run `cabal build` behind the scenes as needed, so
you don't need both.

### Using Stack
Alternatively, if you have Stack installed (another Haskell package management
tool), you may run

    stack test

for the same result.
