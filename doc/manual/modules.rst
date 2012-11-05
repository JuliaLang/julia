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

The following example illustrates the major features of modules::

    module MyModule
    using Base
    
    export MyType, foo
    
    type MyType
        x
    end
    
    bar(x) = 2x
    foo(a::MyType) = bar(a.x) + 1
    
    import Base.show
    show(io, a::MyType) = print(io, "MyType $(a.x)")
    end

Note that the style is not
to indent the body of the module, since that would typically lead to
whole files being indented.

This module defines a type ``MyType``, and two functions. Function ``foo``
and type ``MyType`` are
exported, and so will be available for importing into other modules.
Function ``bar`` is private to ``MyModule``.

The statement ``using Base`` means that the ``Base`` module (which contains
the standard library definitions) will be available for resolving names
as needed. When a global variable is encountered that has no definition in
the current module, the system will search for it in ``Base`` and import it
if it is found there.
This means that all uses of that global within the current module will
resolve to the definition of that variable in ``Base``.

Once a variable is imported this way (or, equivalently, with the ``import``
keyword), a module may not create its own variable with the same name.
Imported variables are read-only; assigning to a global variable always
affects a variable owned by the current module, or else raises an error.

Method definitions are a bit special: they do not search modules named in
``using`` statements. The definition ``function foo()`` creates a new
``foo`` in the current module, unless ``foo`` has already been imported from
elsewhere. For example, in ``MyModule`` above we wanted to add a method
to the standard ``show`` function, so we had to write ``import Base.show``.


Modules and files
-----------------

Files and file names are unrelated to modules; modules are associated only with
module expressions.
One can have multiple files per module, and multiple modules per file::

    module Foo

    include("file1.jl")
    include("file2.jl")

    end

Including the same code in different modules provides mixin-like behavior.
One could use this to run the same code with different base definitions,
for example testing code by running it with "safe" versions of some
operators::

    module Normal
    using Base
    include("mycode.jl")
    end

    module Testing
    include("safe_operators.jl")
    include("mycode.jl")
    end


Standard modules
----------------

There are three important standard modules: Main, Core, and Base.

Main is the top-level module, and Julia starts with Main set as the
current module.
Variables defined at the prompt go in Main, and ``whos()`` lists variables
in Main. Main implicitly contains ``using Base``.

Core contains all identifiers considered "built in" to the language, i.e.
part of the core language and not libraries. Every module implicitly
specifies ``using Core``, since you can't do anything without those
definitions.

Base is the standard library (the contents of base/). This is not imported
by default, so most modules will want to start with ``using Base``.


Miscellaneous details
---------------------

If a name is qualified (e.g. ``Base.sin``), then it can be accessed even if
it is not exported. This is often useful when debugging.

Macros must be exported if they are intended to be used outside their
defining module.
Macro names are written with ``@`` in import and export statements, e.g.
``import Mod.@mac``.

The syntax ``M.x = y`` does not work to assign a global in another module;
global assignment is always module-local.

A variable can be "reserved" for the current module without assigning to
it by declaring it as ``global x`` at the top level. This can be used to
prevent name conflicts for globals initialized after load time.
