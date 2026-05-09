# Build instructions

This project uses stack to manage builds. The provided Makefile can be used to generate an executable in the root folder of the project called `line`.

```sh
# To build `line`
make

# To remove build artifacts
make clean
```
# TODO

- Write the report
- Rewrite language implementation to use inline proofs
- (Stretch) Implement recursive types or Recursion or (If stack machine) Control flow to give (co)inductive types

# Implementation Idea Outline

- SystemT (need frontend)

- Finite types (tuples, "either"/"result" type) and recursors for them

- Add Hindley-Milner polymorphism/inference

- Fixpoints? (if we wan't to be able to have nonterminating programs)

- Linear types, ! modality for non-linear types

## Possible other things

- De Bruijn indecies is a method to avoid accounting for α-equivalence
