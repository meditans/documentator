# Documentator

This project is an exploration of the design space for "(semi)automatic documentation" in haskell.

The approach is multifold:

### Semantic learning order

A preliminary pass could be to analyze the various components (classes and types) which appear in a signature:
for example, if a signature is `(ClassFoo a) => a -> b -> c`, the components are `ClassFoo`, `ClassFoo a => a`, `b` and `c`.
Heuristics can then be used to create a order in which the types ought to be learned.

### Entry/Exit points for a library

An "Entry point" for a library is a function that uses external types and
abstractions to construct an abstraction that lies in the library. This can be
searched for automatically, and is quite useful to answer the question of "what
are the types of the things I can create at first, and how".

Similarly, an "exit point" for a library, can be defined as a function that
returns a value which is out of the abstraction of the library.

### Constructors/Destructors for given types

An "Constructor" for a type answers to the question "How is this type
constructed". How do I obtain a value of type X? Similarly, a "Destructor" is a
function which yields the data contained in a given type.

### Improving heuristics with source analysis

This could be linked to an automatic API to scan the haskell projects on github
and/or the hackage packages, to find and retrieve commonly used functions or
patterns, or to refine the heuristic for the relative importance of functions and types.

## Installing

Please, use the stack.yaml file, as I am using a slightly patched version of `hint`.

## Long term plan

Eventually, we should be able to generate a html proto-tutorial for the library,
and/or integrate the heuristics as a plugint in the new
[haskell-ide project](https://github.com/haskell/haskell-ide).

## Contributing

This project is in its early stage, so feedback or suggestions on desired
functionality or architecture is appreciated.

You can find a design document, written in italian, in the file `design.org`,
intended for the (wonderful) people of Haskell-ITA.

For the later stages, you can send me a pull request, and/or if you're a
beginner, get in touch to be mentored in contributions.
