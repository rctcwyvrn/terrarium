# Terrarium 

A dumb little compiler project, a collection of cute flowers and trees

### What?
It's a compiler from a subset of ocaml to riscv 

### General mindset

Make a highly understandable and debuggable compiler
1. Write something that is highly debuggable and tracable. Compilers are hard to understand so try to encode as much information into the process as possible. We can have nice things!
2. Write as much of the code yourself as possible to really understand how everything works

### Goals
- Write a CPS compiler
- Write/use a monadic parser
- Experiment with memory management (ASAP memory management experiments)
- Implement a basic build system + linker 