# Terrarium 

A dumb little compiler project, a collection of cute flowers

### What?
It's a compiler from a subset of ocaml to ocaml lambda IR and riscv and maybe some other things

### General mindset

Make a highly understandable and debuggable compiler
1. Write something that is highly debuggable and tracable. Compilers are hard to understand so try to encode as much information into the process as possible. We can have nice things!
2. Write as much of the code yourself as possible to really understand how everything works

### Goals
- Write a CPS compiler
- Write/use a monadic parser
- Experiment with memory management
- Implement a basic build system + linker 
- Lots of other things