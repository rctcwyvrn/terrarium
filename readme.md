# Terrarium

Goal: A `rust` library operating system for hosting web servers, much like `MirageOS`, except designed for `qemu` and easy hacking

## Subgoals
- Have it be fast and generate tiny binaries
- Have it be easy to hack on and implement drivers
- Have it be easy to swap out the target device (be that xen or baremetal)

## Non-goals
- Embedded/baremetal considerations (assume we're running on a general purpose hypervisor)
- Reliability (one `panic` should bring down the entire OS)
- Being practical for any use case other than learning

# Milestones

## Library operating system basics
1. Read up on `MirageOS`, `Hubris`, and `HaLVM`

## Bootloader
1. Understand `virtIO`
2. Understand `r0`, and the `chocolate-milk` and `hubris` bootloaders
3. Write something that boots in qemu and writes "hi" to the console device using `virtIO`

## Rust background

1. Get a better grasp on rust `async` (read the async book)
2. Investigate `async` runtimes
3. Get used to writing `nostd`

## First boot test
1. Hook the rust compiler and the bootloader together (ala `mirage-unikraft`)
2. Something that boots in qemu and writes an rust string to the console

## Building libraries
1. Start writing `soil` (implementing an interface around the `virtIO` drivers)
2. (?) Write a keyboard driver (in rust) and its `soil` interface 
3. Write an `echo` program which just takes keyboard input and writes it to display

## Http server
1. Write a network driver (in rust) and its `soil` interface
2. Write a block device driver (in rust) and its `soil` interface
3. Write an entropy provider driver in `soil`
2. Write a http/af backend using `soil`
3. Host a hello world blog post using `terrarium`
