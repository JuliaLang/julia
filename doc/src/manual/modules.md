# [Modules](@id modules)

Modules in Julia are separate variable workspaces, i.e. they introduce a new global scope. They
are delimited syntactically, inside `module Name ... end`. Modules allow you to create top-level
definitions (aka global variables) without worrying about name conflicts when your code is used
together with somebody else's. Within a module, you can control which names from other modules
are visible (via importing), and specify which of your names are intended to be public (via exporting).

The following example demonstrates the major features of modules. It is not meant to be run, but
is shown for illustrative purposes:

```julia
module MyModule
using Lib

using BigLib: thing1, thing2

import Base.show

export MyType, foo

struct MyType
    x
end

bar(x) = 2x
foo(a::MyType) = bar(a.x) + 1

show(io::IO, a::MyType) = print(io, "MyType $(a.x)")
end
```

Note that the style is not to indent the body of the module, since that would typically lead to
whole files being indented.

This module defines a type `MyType`, and two functions. Function `foo` and type `MyType` are exported,
and so will be available for importing into other modules.  Function `bar` is private to `MyModule`.

The statement `using Lib` means that a module called `Lib` will be available for resolving names
as needed. When a global variable is encountered that has no definition in the current module,
the system will search for it among variables exported by `Lib` and import it if it is found there.
This means that all uses of that global within the current module will resolve to the definition
of that variable in `Lib`.

The statement `using BigLib: thing1, thing2` brings just the identifiers `thing1` and `thing2`
into scope from module `BigLib`. If these names refer to functions, adding methods to them
will not be allowed (you may only "use" them, not extend them).

The [`import`](@ref) keyword supports the same syntax as [`using`](@ref).
It does not add modules to be searched the way `using` does. `import` also differs
from `using` in that functions imported using `import` can be extended with new methods.

In `MyModule` above we wanted to add a method to the standard [`show`](@ref) function, so we had to write
`import Base.show`. Functions whose names are only visible via `using` cannot be extended.

Once a variable is made visible via `using` or `import`, a module may not create its own variable
with the same name. Imported variables are read-only; assigning to a global variable always affects
a variable owned by the current module, or else raises an error.

## Summary of module usage

To load a module, two main keywords can be used: `using` and `import`. To understand their differences,
consider the following example:

```julia
module MyModule

export x, y

x() = "x"
y() = "y"
p() = "p"

end
```

In this module we export the `x` and `y` functions (with the keyword [`export`](@ref)), and also have
the non-exported function `p`. There are several different ways to load the Module and its inner
functions into the current workspace:

| Import Command                  | What is brought into scope                                                                  | Available for method extension              |
|:------------------------------- |:------------------------------------------------------------------------------------------- |:------------------------------------------- |
| `using MyModule`                | All `export`ed names (`x` and `y`), `MyModule`, `MyModule.x`, `MyModule.y` and `MyModule.p` | `MyModule.x`, `MyModule.y` and `MyModule.p` |
| `using MyModule: x, p`          | `x` and `p`                                                                                 |                                             |
| `import MyModule`               | `MyModule`, `MyModule.x`, `MyModule.y` and `MyModule.p`                                     | `MyModule.x`, `MyModule.y` and `MyModule.p` |
| `import MyModule.x, MyModule.p` | `x` and `p`                                                                                 | `x` and `p`                                 |
| `import MyModule: x, p`         | `x` and `p`                                                                                 | `x` and `p`                                 |

### Modules and files

Files and file names are mostly unrelated to modules; modules are associated only with module
expressions. One can have multiple files per module, and multiple modules per file:

```julia
module Foo

include("file1.jl")
include("file2.jl")

end
```

Including the same code in different modules provides mixin-like behavior. One could use this
to run the same code with different base definitions, for example testing code by running it with
"safe" versions of some operators:

```julia
module Normal
include("mycode.jl")
end

module Testing
include("safe_operators.jl")
include("mycode.jl")
end
```

### Standard modules

There are three important standard modules:
* [`Core`](@ref) contains all functionality "built into" the language.
* [`Base`](@ref) contains basic functionality that is useful in almost all cases.
* [`Main`](@ref) is the top-level module and the current module, when Julia is started.


!!! note "Standard library modules"
    By default Julia ships with some standard library modules. These behave like regular
    Julia packages except that you don't need to install them explicitly. For example,
    if you wanted to perform some unit testing, you could load the `Test` standard library
    as follows:
    ```julia
    using Test
    ```

### Default top-level definitions and bare modules

In addition to `using Base`, modules also automatically contain
definitions of the [`eval`](@ref) and [`include`](@ref) functions,
which evaluate expressions/files within the global scope of that module.

If these default definitions are not wanted, modules can be defined using the keyword [`baremodule`](@ref)
instead (note: `Core` is still imported, as per above). In terms of `baremodule`, a standard
`module` looks like this:

```
baremodule Mod

using Base

eval(x) = Core.eval(Mod, x)
include(p) = Base.include(Mod, p)

...

end
```

### Relative and absolute module paths

Given the statement `using Foo`, the system consults an internal table of top-level modules
to look for one named `Foo`. If the module does not exist, the system attempts to `require(:Foo)`,
which typically results in loading code from an installed package.

However, some modules contain submodules, which means you sometimes need to access a non-top-level
module. There are two ways to do this. The first is to use an absolute path, for example
`using Base.Sort`. The second is to use a relative path, which makes it easier to import submodules
of the current module or any of its enclosing modules:

```
module Parent

module Utils
...
end

using .Utils

...
end
```

Here module `Parent` contains a submodule `Utils`, and code in `Parent` wants the contents of
`Utils` to be visible. This is done by starting the `using` path with a period. Adding more leading
periods moves up additional levels in the module hierarchy. For example `using ..Utils` would
look for `Utils` in `Parent`'s enclosing module rather than in `Parent` itself.

Note that relative-import qualifiers are only valid in `using` and `import` statements.

### Namespace miscellanea

If a name is qualified (e.g. `Base.sin`), then it can be accessed even if it is not exported.
This is often useful when debugging. It can also have methods added to it by using the qualified
name as the function name. However, due to syntactic ambiguities that arise, if you wish to add
methods to a function in a different module whose name contains only symbols, such as an operator,
`Base.+` for example, you must use `Base.:+` to refer to it. If the operator is more than one
character in length you must surround it in brackets, such as: `Base.:(==)`.

Macro names are written with `@` in import and export statements, e.g. `import Mod.@mac`. Macros
in other modules can be invoked as `Mod.@mac` or `@Mod.mac`.

The syntax `M.x = y` does not work to assign a global in another module; global assignment is
always module-local.

A variable name can be "reserved" without assigning to it by declaring it as `global x`.
This prevents name conflicts for globals initialized after load time.

### Module initialization and precompilation

Large modules can take several seconds to load because executing all of the statements in a module
often involves compiling a large amount of code.
Julia creates precompiled caches of the module to reduce this time.

The incremental precompiled module file are created and used automatically when using `import`
or `using` to load a module.  This will cause it to be automatically compiled the first time
it is imported. Alternatively, you can manually call [`Base.compilecache(modulename)`](@ref). The resulting
cache files will be stored in `DEPOT_PATH[1]/compiled/`. Subsequently, the module is automatically
recompiled upon `using` or `import` whenever any of its dependencies change; dependencies are modules it
imports, the Julia build, files it includes, or explicit dependencies declared by [`include_dependency(path)`](@ref)
in the module file(s).

For file dependencies, a change is determined by examining whether the modification time (`mtime`)
of each file loaded by `include` or added explicitly by `include_dependency` is unchanged, or equal
to the modification time truncated to the nearest second (to accommodate systems that can't copy
mtime with sub-second accuracy). It also takes into account whether the path to the file chosen
by the search logic in `require` matches the path that had created the precompile file. It also takes
into account the set of dependencies already loaded into the current process and won't recompile those
modules, even if their files change or disappear, in order to avoid creating incompatibilities between
the running system and the precompile cache.

If you know that a module is *not* safe to precompile your module
(for example, for one of the reasons described below), you should
put `__precompile__(false)` in the module file (typically placed at the top).
This will cause `Base.compilecache` to throw an error, and will cause `using` / `import` to load it
directly into the current process and skip the precompile and caching.
This also thereby prevents the module from being imported by any other precompiled module.

You may need to be aware of certain behaviors inherent in the creation of incremental shared libraries
which may require care when writing your module. For example, external state is not preserved.
To accommodate this, explicitly separate any initialization steps that must occur at *runtime*
from steps that can occur at *compile time*.
For this purpose, Julia allows you to define an `__init__()` function in your module that executes
any initialization steps that must occur at runtime.
This function will not be called during compilation (`--output-*`).
Effectively, you can assume it will be run exactly once in the lifetime of the code.
You may, of course, call it manually if necessary, but the default is to assume this function deals with computing
state for the local machine, which does not need to be – or even should not be – captured
in the compiled image. It will be called after the module is loaded into a process, including
if it is being loaded into an incremental compile (`--output-incremental=yes`), but not if it
is being loaded into a full-compilation process.

In particular, if you define a `function __init__()` in a module, then Julia will call `__init__()`
immediately *after* the module is loaded (e.g., by `import`, `using`, or `require`) at runtime
for the *first* time (i.e., `__init__` is only called once, and only after all statements in the
module have been executed). Because it is called after the module is fully imported, any submodules
or other imported modules have their `__init__` functions called *before* the `__init__` of the
enclosing module.

Two typical uses of `__init__` are calling runtime initialization functions of external C libraries
and initializing global constants that involve pointers returned by external libraries.  For example,
suppose that we are calling a C library `libfoo` that requires us to call a `foo_init()` initialization
function at runtime. Suppose that we also want to define a global constant `foo_data_ptr` that
holds the return value of a `void *foo_data()` function defined by `libfoo` -- this constant must
be initialized at runtime (not at compile time) because the pointer address will change from run
to run.  You could accomplish this by defining the following `__init__` function in your module:

```julia
const foo_data_ptr = Ref{Ptr{Cvoid}}(0)
function __init__()
    ccall((:foo_init, :libfoo), Cvoid, ())
    foo_data_ptr[] = ccall((:foo_data, :libfoo), Ptr{Cvoid}, ())
    nothing
end
```

Notice that it is perfectly possible to define a global inside a function like `__init__`; this
is one of the advantages of using a dynamic language. But by making it a constant at global scope,
we can ensure that the type is known to the compiler and allow it to generate better optimized
code. Obviously, any other globals in your module that depends on `foo_data_ptr` would also have
to be initialized in `__init__`.

Constants involving most Julia objects that are not produced by [`ccall`](@ref) do not need to be placed
in `__init__`: their definitions can be precompiled and loaded from the cached module image. This
includes complicated heap-allocated objects like arrays. However, any routine that returns a raw
pointer value must be called at runtime for precompilation to work ([`Ptr`](@ref) objects will turn into
null pointers unless they are hidden inside an [`isbits`](@ref) object). This includes the return values
of the Julia functions [`@cfunction`](@ref) and [`pointer`](@ref).

Dictionary and set types, or in general anything that depends on the output of a `hash(key)` method,
are a trickier case.  In the common case where the keys are numbers, strings, symbols, ranges,
`Expr`, or compositions of these types (via arrays, tuples, sets, pairs, etc.) they are safe to
precompile.  However, for a few other key types, such as `Function` or `DataType` and generic
user-defined types where you haven't defined a `hash` method, the fallback `hash` method depends
on the memory address of the object (via its `objectid`) and hence may change from run to run.
If you have one of these key types, or if you aren't sure, to be safe you can initialize this
dictionary from within your `__init__` function. Alternatively, you can use the [`IdDict`](@ref)
dictionary type, which is specially handled by precompilation so that it is safe to initialize
at compile-time.

When using precompilation, it is important to keep a clear sense of the distinction between the
compilation phase and the execution phase. In this mode, it will often be much more clearly apparent
that Julia is a compiler which allows execution of arbitrary Julia code, not a standalone interpreter
that also generates compiled code.

Other known potential failure scenarios include:

1. Global counters (for example, for attempting to uniquely identify objects). Consider the following
   code snippet:

   ```julia
   mutable struct UniquedById
       myid::Int
       let counter = 0
           UniquedById() = new(counter += 1)
       end
   end
   ```

   while the intent of this code was to give every instance a unique id, the counter value is recorded
   at the end of compilation. All subsequent usages of this incrementally compiled module will start
   from that same counter value.

   Note that `objectid` (which works by hashing the memory pointer) has similar issues (see notes
   on `Dict` usage below).

   One alternative is to use a macro to capture [`@__MODULE__`](@ref) and store it alone with the current `counter` value,
   however, it may be better to redesign the code to not depend on this global state.
2. Associative collections (such as `Dict` and `Set`) need to be re-hashed in `__init__`. (In the
   future, a mechanism may be provided to register an initializer function.)
3. Depending on compile-time side-effects persisting through load-time. Example include: modifying
   arrays or other variables in other Julia modules; maintaining handles to open files or devices;
   storing pointers to other system resources (including memory);
4. Creating accidental "copies" of global state from another module, by referencing it directly instead
   of via its lookup path. For example, (in global scope):

   ```julia
   #mystdout = Base.stdout #= will not work correctly, since this will copy Base.stdout into this module =#
   # instead use accessor functions:
   getstdout() = Base.stdout #= best option =#
   # or move the assignment into the runtime:
   __init__() = global mystdout = Base.stdout #= also works =#
   ```

Several additional restrictions are placed on the operations that can be done while precompiling
code to help the user avoid other wrong-behavior situations:

1. Calling [`eval`](@ref) to cause a side-effect in another module. This will also cause a warning to be
   emitted when the incremental precompile flag is set.
2. `global const` statements from local scope after `__init__()` has been started (see issue #12010
   for plans to add an error for this)
3. Replacing a module is a runtime error while doing an incremental precompile.

A few other points to be aware of:

1. No code reload / cache invalidation is performed after changes are made to the source files themselves,
   (including by `Pkg.update`), and no cleanup is done after `Pkg.rm`
2. The memory sharing behavior of a reshaped array is disregarded by precompilation (each view gets
   its own copy)
3. Expecting the filesystem to be unchanged between compile-time and runtime e.g. [`@__FILE__`](@ref)/`source_path()`
   to find resources at runtime, or the BinDeps `@checked_lib` macro. Sometimes this is unavoidable.
   However, when possible, it can be good practice to copy resources into the module at compile-time
   so they won't need to be found at runtime.
4. `WeakRef` objects and finalizers are not currently handled properly by the serializer (this will
   be fixed in an upcoming release).
5. It is usually best to avoid capturing references to instances of internal metadata objects such
   as `Method`, `MethodInstance`, `MethodTable`, `TypeMapLevel`, `TypeMapEntry` and fields of those objects,
   as this can confuse the serializer and may not lead to the outcome you desire. It is not necessarily
   an error to do this, but you simply need to be prepared that the system will try to copy some
   of these and to create a single unique instance of others.

It is sometimes helpful during module development to turn off incremental precompilation. The
command line flag `--compiled-modules={yes|no}` enables you to toggle module precompilation on and
off. When Julia is started with `--compiled-modules=no` the serialized modules in the compile cache
are ignored when loading modules and module dependencies. `Base.compilecache` can still be called
manually. The state of this command line flag is passed to `Pkg.build` to disable automatic
precompilation triggering when installing, updating, and explicitly building packages.
