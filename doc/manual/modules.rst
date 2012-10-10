.. _man-modules:

*********
 Modules  
*********

Modules in Julia are separate global variable workspaces. They are
delimited syntactically, inside ``module Name ... end``. Modules allow
you to create top-level definitions without worrying about name conflicts
when your code is used together with somebody else's. Within a module, you
can control which names from other modules are visible (via importing),
and specify which of your names are intended to be public (via exporting).

One can have multiple files per module, and
multiple modules per file::

    module Foo

    include("file1.jl")
    include("file2.jl")

    end

Note that the style is not
to indent the body of the module, since that would typically lead to
whole files being indented.
Files and file names are unrelated to modules; modules are associated only with
module expressions.

Including the same code in different modules provides mixin-like behavior.
One could use this to run the same code with different base definitions,
for example testing code by running it with "safe" versions of some
operators::

    module Normal
    import Base.*
    include("mycode.jl")
    end

    module Testing
    include("safe_operators.jl")
    include("mycode.jl")
    end

The ``import Base.*`` statement makes all exported identifiers in the Base
module available in the current module.

There are four important standard modules: Root, Core, Base, and Main.

Root is a hidden module containing the loaded top-level modules. You
never need to use it directly; ``import`` always begins its name lookup
in the Root module. ``import Foo`` gives you access to the ``Foo`` identifier
in the Root module, i.e. a reference to the Foo module. You can then
access identifiers in that module as ``Foo.bar``.

Core contains all identifiers considered "built in" to the language, i.e.
part of the core language and not libraries. Every module implicitly
specifies ``import Core.*``, since you can't do anything without those
definitions.

Base is the standard library (the contents of base/). This is not imported
by default, so most modules will want to start with ``import Base.*``.

Main serves as the current open module when Julia starts and you run a
script or begin typing at the prompt. ``whos()`` lists identifiers in Main.
Main has a special behavior: when a ``module`` expression is encountered,
the resulting module is stored in Root. In all other cases a module is
stored within the current module, creating submodules. Main imports ``Base.*``.

The forms of ``import`` implemented so far are ``import Foo``, which imports
a single module, ``import Foo.*`` as discussed, and ``import Foo.a`` which
imports a single identifier from a module. Modules must explicitly
list exported identifiers using ``export a, b, c, ...``.

Currently ``export`` only affects which symbols can be imported into
other modules. All symbols can be accessed if they are qualified, e.g.
Base._jl_something_private.


Method definition and assignment
--------------------------------

Assignment of globals always takes place in the current module.
The syntax ``M.x = y`` does not work to assign a global in another module.
Top-level assignments will also overwrite any imported bindings.
If a function is imported, however, defining methods for it will add
methods to the existing function. This can be overridden by declaring
``global f`` to overwrite the imported binding with a local one. Example::

    module Foo
    import Base.sin
    sin(x::T) = 42   # method added to Base.sin
    end

    module Bar
    import Base.*
    global sin
    sin(x::T) = 42   # a new sin function with one definition
    end

