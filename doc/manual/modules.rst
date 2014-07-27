.. _man-modules:

*********
 Modules  
*********

.. index:: module, baremodule, using, import, export, importall

Modules in Julia are separate global variable workspaces. They are
delimited syntactically, inside ``module Name ... end``. Modules allow
you to create top-level definitions without worrying about name conflicts
when your code is used together with somebody else's. Within a module, you
can control which names from other modules are visible (via importing),
and specify which of your names are intended to be public (via exporting).

The following example demonstrates the major features of modules. It is 
not meant to be run, but is shown for illustrative purposes::

    module MyModule
    using Lib
    
    import BigLib: bar, baz
    
    export MyType, foo
    
    type MyType
        x
    end
    
    bar(x) = 2x
    foo(a::MyType) = bar(a.x) + 1
    
    import Base.show
    show(io, a::MyType) = print(io, "MyType $(a.x)")
    end

Note that the style is not to indent the body of the module, since
that would typically lead to whole files being indented.

This module defines a type ``MyType``, and two functions. Function
``foo`` and type ``MyType`` are exported, and so will be available for
importing into other modules.  Function ``bar`` is private to
``MyModule``.

The statement ``using Lib`` means that a module called ``Lib`` will be
available for resolving names as needed. When a global variable is
encountered that has no definition in the current module, the system
will search for it in ``Lib`` and import it if it is found there.
This means that all uses of that global within the current module will
resolve to the definition of that variable in ``Lib``.

The statement ``import BigLib: bar, baz`` means that the names `bar` and `baz`
from the `BigLib` module will be available as needed (but no other names).

Once a variable is imported this way (or, equivalently, with the ``import``
keyword), a module may not create its own variable with the same name.
Imported variables are read-only; assigning to a global variable always
affects a variable owned by the current module, or else raises an error.

Method definitions are a bit special: they do not search modules named in
``using`` statements. The definition ``function foo()`` creates a new
``foo`` in the current module, unless ``foo`` has already been imported from
elsewhere. For example, in ``MyModule`` above we wanted to add a method
to the standard ``show`` function, so we had to write ``import Base.show``.

Summary of module usage
^^^^^^^^^^^^^^^^^^^^^^^

To load a module, two main keywords can be used: ``using`` and ``import``. To understand their differences, consider the following example::

    module MyModule
    
    export x, y

    x() = "x"
    y() = "y"
    p() = "p"
    
    end

In this module we export the ``x`` and ``y`` functions (with the keyword ``export``), and also have the non-exported function ``p``. There are several different ways to load the Module and its inner functions into the current workspace:

+------------------------------------+----------------------------------------------------------------------------------------------+------------------------------------------------------------------------+
|Import Command                      | What is brought into scope                                                                   | Available for method extension                                         |
+====================================+==============================================================================================+========================================================================+
| ``using MyModule``                 | All ``export`` ed names (``x`` and ``y``), ``MyModule.x``, ``MyModule.y`` and ``MyModule.p`` | ``MyModule.x``, ``MyModule.y`` and ``MyModule.p``                      |
+------------------------------------+----------------------------------------------------------------------------------------------+------------------------------------------------------------------------+
| ``using MyModule.x, MyModule.p``   | ``x`` and ``p``                                                                              |                                                                        |
+------------------------------------+----------------------------------------------------------------------------------------------+------------------------------------------------------------------------+
| ``using MyModule: x, p``           | ``x`` and ``p``                                                                              |                                                                        |
+------------------------------------+----------------------------------------------------------------------------------------------+------------------------------------------------------------------------+
| ``import MyModule``                | ``MyModule.x``, ``MyModule.y`` and ``MyModule.p``                                            | ``MyModule.x``, ``MyModule.y`` and ``MyModule.p``                      |
+------------------------------------+----------------------------------------------------------------------------------------------+------------------------------------------------------------------------+
| ``import MyModule.x, MyModule.p``  | ``x`` and ``p``                                                                              | ``x`` and ``p``                                                        |
+------------------------------------+----------------------------------------------------------------------------------------------+------------------------------------------------------------------------+
| ``import MyModule: x, p``          | ``x`` and ``p``                                                                              | ``x`` and ``p``                                                        |
+------------------------------------+----------------------------------------------------------------------------------------------+------------------------------------------------------------------------+
| ``importall MyModule``             |  All ``export`` ed names (``x`` and ``y``)                                                   | ``x`` and ``y``                                                        |
+------------------------------------+----------------------------------------------------------------------------------------------+------------------------------------------------------------------------+

Module paths
------------

The Julia variable LOAD_PATH contains the directories Julia searches for 
modules. It can be extended using the ``push!`` method::

    push!(LOAD_PATH, "/Path/To/My/Module/")

Putting this statement to the ``~\.juliarc.jl`` file will extend LOAD_PATH 
on every Julia startup. Alternatively, the Julia module load path can be
extended by defining the environoment variable JULIA_LOAD_PATH and putting
directories to it.

Modules and files
-----------------

Files and file names are mostly unrelated to modules; modules are associated
only with module expressions.
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
current module.  Variables defined at the prompt go in Main, and
``whos()`` lists variables in Main.

Core contains all identifiers considered "built in" to the language, i.e.
part of the core language and not libraries. Every module implicitly
specifies ``using Core``, since you can't do anything without those
definitions.

Base is the standard library (the contents of base/). All modules implicitly
contain ``using Base``, since this is needed in the vast majority of cases.


Default top-level definitions and bare modules
----------------------------------------------

In addition to ``using Base``, all operators are explicitly imported,
since one typically wants to extend operators rather than creating entirely
new definitions of them. A module also automatically contains a definition
of the ``eval`` function, which evaluates expressions within the context of
that module.

If these definitions are not wanted, modules can be defined using the
keyword ``baremodule`` instead. In terms of ``baremodule``, a standard
``module`` looks like this::

    baremodule Mod

    using Base

    importall Base.Operators

    eval(x) = Core.eval(Mod, x)
    eval(m,x) = Core.eval(m, x)

    ...

    end


Relative and Absolute Module Paths
----------------------------------

Given the statement ``using Foo``, the system looks for ``Foo``
within ``Main``. If the module does not exist, the system
attempts to ``require("Foo")``, which typically results in loading
code from an installed package.

However, some modules contain submodules, which means you sometimes
need to access a module that is not directly available in ``Main``.
There are two ways to do this. The first is to use an absolute path,
for example ``using Base.Sort``. The second is to use a relative path,
which makes it easier to import submodules of the current module or
any of its enclosing modules::

    module Parent

    module Utils
    ...
    end

    using .Utils

    ...
    end

Here module ``Parent`` contains a submodule ``Utils``, and code in
``Parent`` wants the contents of ``Utils`` to be visible. This is
done by starting the ``using`` path with a period. Adding more leading
periods moves up additional levels in the module hierarchy. For example
``using ..Utils`` would look for ``Utils`` in ``Parent``'s enclosing
module rather than in ``Parent`` itself.


Miscellaneous details
---------------------

If a name is qualified (e.g. ``Base.sin``), then it can be accessed even if
it is not exported. This is often useful when debugging.

Macros must be exported if they are intended to be used outside their
defining module.  Macro names are written with ``@`` in import and
export statements, e.g.  ``import Mod.@mac``.

The syntax ``M.x = y`` does not work to assign a global in another module;
global assignment is always module-local.

A variable can be "reserved" for the current module without assigning to
it by declaring it as ``global x`` at the top level. This can be used to
prevent name conflicts for globals initialized after load time.
