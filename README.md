# Build Instructions

Run `cabal build` to build the project and `cabal run Crisp` to run it

# Running a single module in ghci REPL

First run `cabal repl`, then do `:set -isrc` followed by loading the module. For example, `:l src/Parser.hs`

<!-- # Further plans

- extensive testing
- Support for basic IO
- Executing scripts
- Add loops (and if possible structs)
- make the REPL more feature rich (maybe allow viewing the AST)
- provide better error messages -->