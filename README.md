
# Datalog

A Datalog implementation written in OCaml.

The implementation seeks to combine a number of features from other Datalogs, including:

- Type inference [[1]](#1)
- Automatic subgoal scheduling [[2]](#2)
- Module system [[3]](#3) [[4]](#4)
- Staged compilation [[5]](#5)

# Components 

## Source 

A typed source representation for Datalog supporting user defined subtypes, 
extralogical predicates, negation of body literals and  disjunction of body literals. 

## Parser

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
- Type inference

Roadmap includes:

- Predicate inlining

## Bottom-up Backend

The staged bottom-up backend will take advantage of staging and compile time 
knowledge-base to generate an efficient, compiled program accepting further

This will include:

- Demand transformation
- Linearization
- Compiltation to Relation Abstract Machine

## Top-down Backend 

TODO

# References

<a id="1">[1]</a>
Moor, Oege de, Damien Sereni, Pavel Avgustinov and Mathieu Verbaere. “Type inference for datalog and its application to query optimisation.” PODS (2008).

<a id="2">[2]</a>
Contrastin, Mistral, Dominic A. Orchard and Andrew S C Rice. “Automatic Reordering for Dataflow Safety of Datalog.” Proceedings of the 20th International Symposium on Principles and Practice of Declarative Programming (2018): n. pag.

<a id="3">[3]</a>
https://github.com/bobatkey/modulog

<a id="4">[4]</a>
Leroy, Xavier. “A modular module system.” J. Funct. Program. 10 (2000): 269-303.

<a id="5">[5]</a>
Scholz, Bernhard, Herbert Jordan, Pavle Subotic and Till Westmann. “On fast large-scale program analysis in Datalog.” CC 2016 (2016).
