

# Tina Programming Language


![Build](https://github.com/ebresafegaga/tina/actions/workflows/test.yml/badge.svg)

## Welcome to Tina

Tina is a programming language based algebraic effects and handlers.
This implementation contains an interpreter and two compilers (JavaScript and LLVM)

While algebraic effects and handlers might be the central concept
aroun Tina, it also supports other features such as pattern matching,
closures, algebraic data types, and a type system.


## Contributing to Tina

Everyone is free to contribute!

## Getting Started

There is an online playground comming soon!

## Building

Building Tina is relatively straight forward once you have the
required toolchains installed.

You would need:
    - Dune
    - OCaml (>= 4.12)
    - Opam

Once you have them installed, you can the build it like so:

     Clone this repo:
     ```$ git clone https://github.com/ebresafegaga/tina```

     Pin it to the this version
     ```$ opam pin add tina.dev -n .```

     Install native dependecies 
      ```$ opam depext -yt tina```

      Install library dependecies
      ```$ opam install -t . --deps-only```

      Build the whole project
      ```$ dune build```

