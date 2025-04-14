# [Modules](@id modules)

Modules in Julia help organize code into coherent units. They are delimited syntactically inside
`module NameOfModule ... end`, and have the following features:

1. Modules are separate namespaces, each introducing a new global scope. This is useful, because it
   allows the same name to be used for different functions or global variables without conflict, as long as they are in separate modules.

2. Modules have facilities for detailed namespace management: each defines a set of names it
   `export`s and marks as `public`, and can import names from other modules with `using` and
   `import` (we explain these below).

3. Modules can be precompiled for faster loading, and may contain code for runtime initialization.

Typically, in larger Julia packages you will see module code organized into files, eg

```julia
module SomeModule

# export, public, using, import statements are usually here; we discuss these below

include("file1.jl")
include("file2.jl")

end
```

Files and file names are mostly unrelated to modules; modules are associated only with module
expressions. One can have multiple files per module, and multiple modules per file. `include`
behaves as if the contents of the source file were evaluated in the global scope of the
including module. In this chapter, we use short and simplified examples, so we won't use `include`.

The recommended style is not to indent the body of the module, since that would typically lead to
whole files being indented. Also, it is common to use `UpperCamelCase` for module names (just like
types), and use the plural form if applicable, especially if the module contains a similarly named
identifier, to avoid name clashes. For example,

```julia
module FastThings

struct FastThing
    ...
end

end
```

## [Namespace management](@id namespace-management)

Namespace management refers to the facilities the language offers for making names in a module
available in other modules. We discuss the related concepts and functionality below in detail.

### Qualified names

Names for functions, variables and types in the global scope like `sin`, `ARGS`, and
`UnitRange` always belong to a module, called the *parent module*, which can be found
interactively with [`parentmodule`](@ref), for example

```jldoctest
julia> parentmodule(UnitRange)
Base
```

One can also refer to these names outside their parent module by prefixing them with their module,
eg `Base.UnitRange`. This is called a *qualified name*. The parent module may be accessible using a
chain of submodules like `Base.Math.sin`, where `Base.Math` is called the *module path*.
Due to syntactic ambiguities, qualifying a name that contains only symbols, such as an operator,
requires inserting a colon, e.g. `Base.:+`. A small number of operators additionally require
parentheses, e.g. `Base.:(==)`.

If a name is qualified, then it is always *accessible*, and in case of a function, it can also have
methods added to it by using the qualified name as the function name.

Within a module, a variable name can be “reserved” without assigning to it by declaring it as
`global x`. This prevents name conflicts for globals initialized after load time. The syntax
`M.x = y` does not work to assign a global in another module; global assignment is always
module-local.

### Export lists

Names (referring to functions, types, global variables, and constants) can be added to the
*export list* of a module with `export`: these are the symbols that are imported when `using` the module. Typically, they are at or near the top of the module definition
so that readers of the source code can find them easily, as in

```jldoctest module_manual
julia> module NiceStuff
       export nice, DOG
       struct Dog end      # singleton type, not exported
       const DOG = Dog()   # named instance, exported
       nice(x) = "nice $x" # function, exported
       end;

```

but this is just a style suggestion — a module can have multiple `export` statements in arbitrary
locations.

It is common to export names which form part of the API (application programming interface). In
the above code, the export list suggests that users should use `nice` and `DOG`. However, since
qualified names always make identifiers accessible, this is just an option for organizing APIs:
unlike other languages, Julia has no facilities for truly hiding module internals.

Also, some modules don't export names at all. This is usually done if they use common
words, such as `derivative`, in their API, which could easily clash with the export lists of other
modules. We will see how to manage name clashes below.

To mark a name as public without exporting it into the namespace of folks who call `using NiceStuff`,
one can use `public` instead of `export`. This marks the public name(s) as part of the public API,
but does not have any namespace implications. The `public` keyword is only available in Julia 1.11
and above. To maintain compatibility with Julia 1.10 and below, use the `@compat` macro from the
[Compat](https://github.com/JuliaLang/Compat.jl) package, or the version-aware construct

```julia
VERSION >= v"1.11.0-DEV.469" && eval(Meta.parse("public a, b, c"))
```

`export` is a keyword wherever it occurs whereas the `public` keyword is currently limited to the
syntactic top level within a file or module. This limitation exists for compatibility reasons,
as `public` was introduced as a new keyword in Julia 1.11 while `export` has existed since Julia
1.0. However, this restriction on `public` may be lifted in future releases, so do not use `public`
as an identifier.

### Standalone `using` and `import`

For interactive use, the most common way of loading a module is `using ModuleName`. This [loads](@ref
code-loading) the code associated with `ModuleName`, and brings

1. the module name

2. and the elements of the export list into the surrounding global namespace.

Technically, the statement `using ModuleName` means that a module called `ModuleName` will be
available for resolving names as needed. When a global variable is encountered that has no
definition in the current module, the system will search for it among variables exported by `ModuleName`
and use it if it is found there. This means that all uses of that global within the current
module will resolve to the definition of that variable in `ModuleName`.

To load a module from a package, the statement `using ModuleName` can be used.
To load a module from a locally defined module, a dot needs to be added before the module name like `using .ModuleName`.

To continue with our example,

```jldoctest module_manual
julia> using .NiceStuff
```

would load the above code, making `NiceStuff` (the module name), `DOG` and `nice` available. `Dog` is not on the export list, but it can be accessed if the name is qualified with the module path (which here is just the module name) as `NiceStuff.Dog`.

Importantly, **`using ModuleName` is the only form for which export lists matter at all**.

In contrast,

```jldoctest module_manual
julia> import .NiceStuff
```

brings *only* the module name into scope. Users would need to use `NiceStuff.DOG`, `NiceStuff.Dog`, and `NiceStuff.nice` to access its contents. Usually, `import ModuleName` is used in contexts when the user wants to keep the namespace clean.
As we will see in the next section `import .NiceStuff` is equivalent to `using .NiceStuff: NiceStuff`.

You can combine multiple `using` and `import` statements of the same kind in a comma-separated expression, e.g.

```jldoctest module_manual
julia> using LinearAlgebra, Random
```

### `using` and `import` with specific identifiers, and adding methods

When `using ModuleName:` or `import ModuleName:` is followed by a comma-separated list of names, the module is loaded, but *only those specific names are brought into the namespace* by the statement. For example,

```jldoctest module_manual
julia> using .NiceStuff: nice, DOG
```

will import the names `nice` and `DOG`.

Importantly, the module name `NiceStuff` will *not* be in the namespace. If you want to make it accessible, you have to list it explicitly, as
```jldoctest module_manual
julia> using .NiceStuff: nice, DOG, NiceStuff
```

When two or more packages/modules export a name and that name does not refer to the
same thing in each of the packages, and the packages are loaded via `using` without
an explicit list of names, it is an error to reference that name without qualification.
It is thus recommended that code intended to be forward-compatible with future versions
of its dependencies and of Julia, e.g., code in released packages, list the names it
uses from each loaded package, e.g., `using Foo: Foo, f` rather than `using Foo`.

Julia has two forms for seemingly the same thing because only `import ModuleName: f` allows adding methods to `f`
*without a module path*.
That is to say, the following example will give an error:

```jldoctest module_manual
julia> using .NiceStuff: nice

julia> struct Cat end

julia> nice(::Cat) = "nice 😸"
ERROR: invalid method definition in Main: function NiceStuff.nice must be explicitly imported to be extended
Stacktrace:
 [1] top-level scope
   @ none:1
```

This error prevents accidentally adding methods to functions in other modules that you only intended to use.

There are two ways to deal with this. You can always qualify function names with a module path:
```jldoctest module_manual
julia> using .NiceStuff

julia> struct Cat end

julia> NiceStuff.nice(::Cat) = "nice 😸"
```

Alternatively, you can `import` the specific function name:
```jldoctest module_manual
julia> import .NiceStuff: nice

julia> struct Mouse end

julia> nice(::Mouse) = "nice 🐭"
nice (generic function with 3 methods)
```

Which one you choose is a matter of style. The first form makes it clear that you are adding a
method to a function in another module (remember, that the imports and the method definition may be
in separate files), while the second one is shorter, which is especially convenient if you are
defining multiple methods.

Once a variable is made visible via `using` or `import`, a module may not create its own variable
with the same name. Imported variables are read-only; assigning to a global variable always affects
a variable owned by the current module, or else raises an error.

### Renaming with `as`

An identifier brought into scope by `import` or `using` can be renamed with the keyword `as`.
This is useful for working around name conflicts as well as for shortening names.
For example, `Base` exports the function name `read`, but the CSV.jl package also provides `CSV.read`.
If we are going to invoke CSV reading many times, it would be convenient to drop the `CSV.` qualifier.
But then it is ambiguous whether we are referring to `Base.read` or `CSV.read`:

```julia-repl
julia> read;

julia> import CSV: read
WARNING: ignoring conflicting import of CSV.read into Main
```

Renaming provides a solution:

```julia-repl
julia> import CSV: read as rd
```

Imported packages themselves can also be renamed:

```julia
import BenchmarkTools as BT
```

`as` works with `using` only when a single identifier is brought into scope.
For example `using CSV: read as rd` works, but `using CSV as C` does not, since it operates
on all of the exported names in `CSV`.

### Mixing multiple `using` and `import` statements

When multiple `using` or `import` statements of any of the forms above are used, their effect is combined in the order they appear.
For example,

```jldoctest module_manual
julia> using .NiceStuff         # exported names and the module name

julia> import .NiceStuff: nice  # allows adding methods to unqualified functions

```

would bring all the exported names of `NiceStuff` and the module name itself into scope, and also
allow adding methods to `nice` without prefixing it with a module name.

### Handling name conflicts

Consider the situation where two (or more) packages export the same name, as in

```jldoctest module_manual
julia> module A
       export f
       f() = 1
       end
A
julia> module B
       export f
       f() = 2
       end
B
```

The statement `using .A, .B` works, but when you try to call `f`, you get an error with a hint

```jldoctest module_manual
julia> using .A, .B

julia> f
ERROR: UndefVarError: `f` not defined in `Main`
Hint: It looks like two or more modules export different bindings with this name, resulting in ambiguity. Try explicitly importing it from a particular module, or qualifying the name with the module it should come from.
```

Here, Julia cannot decide which `f` you are referring to, so you have to make a choice. The following solutions are commonly used:

1. Simply proceed with qualified names like `A.f` and `B.f`. This makes the context clear to the reader of your code, especially if `f` just happens to coincide but has different meaning in various packages. For example, `degree` has various uses in mathematics, the natural sciences, and in everyday life, and these meanings should be kept separate.

2. Use the `as` keyword above to rename one or both identifiers, eg

   ```jldoctest module_manual
   julia> using .A: f as f

   julia> using .B: f as g

   ```

   would make `B.f` available as `g`. Here, we are assuming that you did not use `using A` before,
   which would have brought `f` into the namespace.

3. When the names in question *do* share a meaning, it is common for one module to import it from another, or have a lightweight “base” package with the sole function of defining an interface like this, which can be used by other packages. It is conventional to have such package names end in `...Base` (which has nothing to do with Julia's `Base` module).

### Default top-level definitions and bare modules

Modules automatically contain `using Core`, `using Base`, and definitions of the [`eval`](@ref)
and [`include`](@ref) functions, which evaluate expressions/files within the global scope of that
module.

If these default definitions are not wanted, modules can be defined using the keyword
[`baremodule`](@ref) instead (note: `Core` is still imported). In terms of
`baremodule`, a standard `module` looks like this:

```
baremodule Mod

using Base

eval(x) = Core.eval(Mod, x)
include(p) = Base.include(Mod, p)

...

end
```

If even `Core` is not wanted, a module that imports nothing and defines no names at all can be defined with `Module(:YourNameHere, false, false)` and code can be evaluated into it with [`@eval`](@ref) or [`Core.eval`](@ref):
```jldoctest
julia> arithmetic = Module(:arithmetic, false, false)
Main.arithmetic

julia> @eval arithmetic add(x, y) = $(+)(x, y)
add (generic function with 1 method)

julia> arithmetic.add(12, 13)
25
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

## Submodules and relative paths

Modules can contain *submodules*, nesting the same syntax `module ... end`. They can be used to introduce separate namespaces, which can be helpful for organizing complex codebases. Note that each `module` introduces its own [scope](@ref scope-of-variables), so submodules do not automatically “inherit” names from their parent.

It is recommended that submodules refer to other modules within the enclosing parent module (including the latter) using *relative module qualifiers* in `using` and `import` statements. A relative module qualifier starts with a period (`.`), which corresponds to the current module, and each successive `.` leads to the parent of the current module. This should be followed by modules if necessary, and eventually the actual name to access, all separated by `.`s. As a special case, however, referring to the module root can be written without `.`, avoiding the need to count the depth to reach that module.

Consider the following example, where the submodule `SubA` defines a function, which is then extended in its “sibling” module:

```jldoctest module_manual
julia> module ParentModule
       module SubA
       export add_D  # exported interface
       const D = 3
       add_D(x) = x + D
       end
       using .SubA  # brings `add_D` into the namespace
       export add_D # export it from ParentModule too
       module SubB
       import ..SubA: add_D # relative path for a “sibling” module
       # import ParentModule.SubA: add_D # when in a package, such as when this is loaded by using or import, this would be equivalent to the previous import, but not at the REPL
       struct Infinity end
       add_D(x::Infinity) = x
       end
       end;

```

You may see code in packages, which, in a similar situation, uses import without the `.`:
```jldoctest
julia> import ParentModule.SubA: add_D
ERROR: ArgumentError: Package ParentModule not found in current path.
```
However, since this operates through [code loading](@ref code-loading), it only works if `ParentModule` is in a package in a file. If `ParentModule` was defined at the REPL, it is necessary to use use relative paths:
```jldoctest module_manual
julia> import .ParentModule.SubA: add_D

```

Note that the order of definitions also matters if you are evaluating values. Consider

```julia
module TestPackage

export x, y

x = 0

module Sub
using ..TestPackage
z = y # ERROR: UndefVarError: `y` not defined in `Main`
end

y = 1

end
```

where `Sub` is trying to use `TestPackage.y` before it was defined, so it does not have a value.

For similar reasons, you cannot use a cyclic ordering:

```julia
module A

module B
using ..C # ERROR: UndefVarError: `C` not defined in `Main.A`
end

module C
using ..B
end

end
```

## Module initialization and precompilation

Large modules can take several seconds to load because executing all of the statements in a module
often involves compiling a large amount of code.
Julia creates precompiled caches of the module to reduce this time.

Precompiled module files (sometimes called "cache files") are created and used automatically when `import` or `using` loads a module. If the cache file(s) do not yet exist, the module will be compiled and saved for future reuse. You can also manually call [`Base.compilecache(Base.identify_package("modulename"))`](@ref) to create these files without loading the module. The resulting
cache files will be stored in the `compiled` subfolder of `DEPOT_PATH[1]`. If nothing about your system changes,
such cache files will be used when you load the module with `import` or `using`.

Precompilation cache files store definitions of modules, types, methods, and constants. They may also store method specializations and the code generated for them, but this typically requires that the developer add explicit [`precompile`](@ref) directives or execute workloads that force compilation during the package build.

However, if you update the module's dependencies or change its source code, the module is automatically
recompiled upon `using` or `import`. Dependencies are modules it
imports, the Julia build, files it includes, or explicit dependencies declared by [`include_dependency(path)`](@ref)
in the module file(s).

For file dependencies loaded by `include`, a change is determined by examining whether the
file size (`fsize`) or content (condensed into a hash) is unchanged.
For file dependencies loaded by `include_dependency` a change is determined by examining whether the modification time (`mtime`)
is unchanged, or equal to the modification time truncated to the nearest second
(to accommodate systems that can't copy mtime with sub-second accuracy).
It also takes into account whether the path to the file chosen
by the search logic in `require` matches the path that had created the precompile file. It also takes
into account the set of dependencies already loaded into the current process and won't recompile those
modules, even if their files change or disappear, in order to avoid creating incompatibilities between
the running system and the precompile cache.
Finally, it takes account of changes in any [compile-time preferences](@ref preferences).

If you know that a module is *not* safe to precompile
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
or other imported modules have their `__init__` functions called *before* the `__init__` of
the enclosing module. This is also synchronized across threads, so that code can safely rely upon
this ordering of effects, such that all `__init__` will have run, in dependency ordering,
before the `using` result is completed. They may run concurrently with other `__init__`
methods which are not dependencies however, so be careful when accessing any shared state
outside the current module to use locks when needed.

Two typical uses of `__init__` are calling runtime initialization functions of external C libraries
and initializing global constants that involve pointers returned by external libraries. For example,
suppose that we are calling a C library `libfoo` that requires us to call a `foo_init()` initialization
function at runtime. Suppose that we also want to define a global constant `foo_data_ptr` that
holds the return value of a `void *foo_data()` function defined by `libfoo` -- this constant must
be initialized at runtime (not at compile time) because the pointer address will change from run
to run. You could accomplish this by defining the following `__init__` function in your module:

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

It is sometimes helpful during module development to turn off incremental precompilation.
The command line flag `--compiled-modules={yes|no|existing}` enables you to toggle module
precompilation on and off. When Julia is started with `--compiled-modules=no` the serialized
modules in the compile cache are ignored when loading modules and module dependencies. In
some cases, you may want to load existing precompiled modules, but not create new ones. This
can be done by starting Julia with `--compiled-modules=existing`. More fine-grained control
is available with `--pkgimages={yes|no|existing}`, which only affects native-code storage
during precompilation. `Base.compilecache` can still be called manually. The state of this
command line flag is passed to `Pkg.build` to disable automatic precompilation triggering
when installing, updating, and explicitly building packages.

You can also debug some precompilation failures with environment variables. Setting
`JULIA_VERBOSE_LINKING=true` may help resolve failures in linking shared libraries of
compiled native code. See the **Developer Documentation** part of the Julia manual, where
you will find further details in the section documenting Julia's internals under "Package
Images".
