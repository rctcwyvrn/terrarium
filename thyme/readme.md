# thyme 

A monadic parser with a focus on debuggability and propagating errors nicely

You'll see it forces the usage `[%here]` whenever you sequence parsers together, so you form a coherently set of tags on each of your parser atoms

Using `add_info` and `tag` are highly encouraged