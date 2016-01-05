# Tellurium
*Not quite metal itself; added to metals to make them easier to work with. Handle with care.*

Tellurium is an experimental programming language whose philosophy is outlined [here](http://scrivulet.com/projects/tellurium.html).

"Experimental" currently means, in this case, "almost not a sad joke". It has no real type checking, and is probably fragile as heck. But you can define functions, and interface with functions from assembly as long as they comply with the dinky calling convention (args in `eax`, `ecx` and `edx`, return in `eax`). The `second*.te` files are typical of the kind of code that can be compiled.
