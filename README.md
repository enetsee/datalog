
# Datalog

A Datalog implementation written in OCaml offering.

# Components 

## Source 

A typed source representation for Datalog supporting user defined subtypes
and union types and extralogical predicates. 

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

An untyped intermediate representation, program analysis and optimization.

Current features include:

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

