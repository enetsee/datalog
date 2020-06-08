
# Datalog

A Datalog implementation written in OCaml offering.

# Components 

## Source 

A typed source representation for Datalog supporting user defined subtypes
and union types, extralogical predicates, negation of body literals and 
disjunction of body literals. 

The`Source` repesentation compiles to the `Raw` intermediate representation in 
Core.

Roadmap includes:

- input, output and inline declarations
- type inference 
- CTL operators, based on Mistral Contrastin's [Temporalog](https://github.com/madgen/temporalog)
- interface files & imports

## Frontend

A Menhir based incremental parser for the source representation.

Roadmap includes:

- error recovery

## Core 

Untyped intermediate representations, program analysis and optimization.

Core contains 3 intermediate representations:

- `Raw`: Datalog¬
- `Adorned`: `Raw` reprentation with binding pattern annotations
- `Stratified`: a sequence of semi-positive Datalog programs

Analyses and optimizations include:

- Dead clause elimination
- Range restriction repair 
- Automatic subgoal-scheduling
- Generalized adornment transform
- Stratification

Roadmap includes:

- Predicate inlining

## Bottom-up Backend

TODO

This will include:

- Demand transformation
- Linearization

## Top-down Backend 

TODO

## Staged Bottom-up Backend

TODO

The staged bottom-up backend will take advantage of staging and compile time 
knowledge-base to generate an efficient, compiled program accepting further

