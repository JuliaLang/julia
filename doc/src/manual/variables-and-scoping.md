# [Scope of Variables](@id scope-of-variables)

The *scope* of a variable is the region of code within which a variable is visible. Variable scoping
helps avoid variable naming conflicts. The concept is intuitive: two functions can both have arguments
called `x` without the two `x`'s referring to the same thing. Similarly there are many other cases
where different blocks of code can use the same name without referring to the same thing. The
rules for when the same variable name does or doesn't refer to the same thing are called scope
rules; this section spells them out in detail.

Certain constructs in the language introduce *scope blocks*, which are regions of code that are
eligible to be the scope of some set of variables. The scope of a variable cannot be an arbitrary
set of source lines; instead, it will always line up with one of these blocks. There are two
main types of scopes in Julia, *global scope* and *local scope*, the latter can be nested. The
constructs introducing scope blocks are:

# [](@id man-scope-table)

  * Scope blocks that may nest only in other global scope blocks:

    - global scope

      + module, baremodule

      + at interactive prompt (REPL)

    - local scope (don't allow nesting)

      + (mutable) struct, macro

  * Scope blocks which may nest anywhere (in global or local scope):

    - local scope

      + for, while, try-catch-finally, let

      + functions (either syntax, anonymous & do-blocks)

      + comprehensions, broadcast-fusing

Notably missing from this table are
[begin blocks](@ref man-compound-expressions) and [if blocks](@ref man-conditional-evaluation)
which do *not* introduce new scope blocks.
Both types of scopes follow somewhat different rules which will be explained below.

Julia uses [lexical scoping](https://en.wikipedia.org/wiki/Scope_%28computer_science%29#Lexical_scoping_vs._dynamic_scoping),
meaning that a function's scope does not inherit from its caller's scope, but from the scope in
which the function was defined. For example, in the following code the `x` inside `foo` refers
to the `x` in the global scope of its module `Bar`:

```jldoctest moduleBar
julia> module Bar
           x = 1
           foo() = x
       end;
```

and not a `x` in the scope where `foo` is used:

```jldoctest moduleBar
julia> import .Bar

julia> x = -1;

julia> Bar.foo()
1
```

Thus *lexical scope* means that the scope of variables can be inferred from the source code alone.

## Global Scope

Each module introduces a new global scope, separate from the global scope of all other modules;
there is no all-encompassing global scope. Modules can introduce variables of other modules into
their scope through the [using or import](@ref modules) statements or through qualified access using the
dot-notation, i.e. each module is a so-called *namespace*. Note that variable bindings can only
be changed within their global scope and not from an outside module.

```jldoctest
julia> module A
           a = 1 # a global in A's scope
       end;

julia> module B
           module C
               c = 2
           end
           b = C.c    # can access the namespace of a nested global scope
                      # through a qualified access
           import ..A # makes module A available
           d = A.a
       end;

julia> module D
           b = a # errors as D's global scope is separate from A's
       end;
ERROR: UndefVarError: a not defined

julia> module E
           import ..A # make module A available
           A.a = 2    # throws below error
       end;
ERROR: cannot assign variables in other modules
```

Note that the interactive prompt (aka REPL) is in the global scope of the module `Main`.

## Local Scope

A new local scope is introduced by most code blocks (see above
[table](@ref man-scope-table) for a complete list).
A local scope inherits all the variables from a parent local scope,
both for reading and writing.
Additionally, the local scope inherits all globals that are assigned
to in its parent global scope block (if it is surrounded by a global `if` or `begin` scope).
Unlike global scopes, local scopes are not namespaces,
thus variables in an inner scope cannot be retrieved from the parent scope through some sort of
qualified access.

The following rules and examples pertain to local scopes.
A newly introduced variable in a local scope does not
back-propagate to its parent scope.
For example, here the ``z`` is not introduced into the top-level scope:

```jldoctest
julia> for i = 1:10
           z = i
       end

julia> z
ERROR: UndefVarError: z not defined
```

(Note, in this and all following examples it is assumed that their top-level is a global scope
with a clean workspace, for instance a newly started REPL.)

Inside a local scope a variable can be forced to be a new local variable using the `local` keyword:

```jldoctest
julia> x = 0;

julia> for i = 1:10
           local x # this is also the default
           x = i + 1
       end

julia> x
0
```

Inside a local scope a global variable can be assigned to by using the keyword `global`:

```jldoctest
julia> for i = 1:10
           global z
           z = i
       end

julia> z
10
```

The location of both the `local` and `global` keywords within the scope block is irrelevant.
The following is equivalent to the last example (although stylistically worse):

```jldoctest
julia> for i = 1:10
           z = i
           global z
       end

julia> z
10
```

The `local` and `global` keywords can also be applied to destructuring assignments, e.g.
`local x, y = 1, 2`. In this case the keyword affects all listed variables.

Local scopes are introduced by most block keywords,
with notable exceptions of `begin` and `if`.

In a local scope, all variables are inherited from its parent
global scope block unless:

  * an assignment would result in a modified *global* variable, or
  * a variable is specifically marked with the keyword `local`.

Thus global variables are only inherited for reading but not for writing:

```jldoctest
julia> x, y = 1, 2;

julia> function foo()
           x = 2        # assignment introduces a new local
           return x + y # y refers to the global
       end;

julia> foo()
4

julia> x
1
```

An explicit `global` is needed to assign to a global variable:

!!! sidebar "Avoiding globals"
    Avoiding changing the value of global variables is considered by many
    to be a programming best-practice.
    One reason for this is that remotely changing the state of global variables in other
    modules should be done with care as it makes the local behavior of the program hard to reason about.
    This is why the scope blocks that introduce local scope require the ``global``
    keyword to declare the intent to modify a global variable.

```jldoctest
julia> x = 1;

julia> function foobar()
           global x = 2
       end;

julia> foobar();

julia> x
2
```

Note that *nested functions* can modify their parent scope's *local* variables:

```jldoctest
julia> x, y = 1, 2;

julia> function baz()
           x = 2 # introduces a new local
           function bar()
               x = 10       # modifies the parent's x
               return x + y # y is global
           end
           return bar() + x # 12 + 10 (x is modified in call of bar())
       end;

julia> baz()
22

julia> x, y # verify that global x and y are unchanged
(1, 2)
```

The reason to allow *modifying local* variables of parent scopes in
nested functions is to allow constructing [`closures`](https://en.wikipedia.org/wiki/Closure_%28computer_programming%29)
which have a private state, for instance the ``state`` variable in the
following example:

```jldoctest
julia> let state = 0
           global counter() = (state += 1)
       end;

julia> counter()
1

julia> counter()
2
```

See also the closures in the examples in the next two sections. A variable
such as `x` in the first example and `state` in the second that is inherited
from the enclosing scope by the inner function is sometimes called a
*captured* variable. Captured variables can present performance challenges
discussed in [performance tips](@ref man-performance-tips).

The distinction between inheriting global scope and nesting local scope
can lead to some slight differences between functions
defined in local vs. global scopes for variable assignments.
Consider the modification of the last example by moving `bar` to the global scope:

```jldoctest
julia> x, y = 1, 2;

julia> function bar()
           x = 10 # local, no longer a closure variable
           return x + y
       end;

julia> function quz()
           x = 2 # local
           return bar() + x # 12 + 2 (x is not modified)
       end;

julia> quz()
14

julia> x, y # verify that global x and y are unchanged
(1, 2)
```

Note that the above nesting rules do not pertain to type and macro definitions as they can only appear
at the global scope. There are special scoping rules concerning the evaluation of default and
keyword function arguments which are described in the [Function section](@ref man-functions).

An assignment introducing a variable used inside a function, type or macro definition need not
come before its inner usage:

```jldoctest
julia> f = y -> y + a;

julia> f(3)
ERROR: UndefVarError: a not defined
Stacktrace:
[...]

julia> a = 1
1

julia> f(3)
4
```

This behavior may seem slightly odd for a normal variable, but allows for named functions -- which
are just normal variables holding function objects -- to be used before they are defined. This
allows functions to be defined in whatever order is intuitive and convenient, rather than forcing
bottom up ordering or requiring forward declarations, as long as they are defined by the time
they are actually called. As an example, here is an inefficient, mutually recursive way to test
if positive integers are even or odd:

```jldoctest
julia> even(n) = (n == 0) ? true : odd(n - 1);

julia> odd(n) = (n == 0) ? false : even(n - 1);

julia> even(3)
false

julia> odd(3)
true
```

Julia provides built-in, efficient functions to test for oddness and evenness called [`iseven`](@ref)
and [`isodd`](@ref) so the above definitions should only be considered to be examples of scope,
not efficient design.

### Let Blocks

Unlike assignments to local variables, `let` statements allocate new variable bindings each time
they run. An assignment modifies an existing value location, and `let` creates new locations.
This difference is usually not important, and is only detectable in the case of variables that
outlive their scope via closures. The `let` syntax accepts a comma-separated series of assignments
and variable names:

```jldoctest
julia> x, y, z = -1, -1, -1;

julia> let x = 1, z
           println("x: $x, y: $y") # x is local variable, y the global
           println("z: $z") # errors as z has not been assigned yet but is local
       end
x: 1, y: -1
ERROR: UndefVarError: z not defined
```

The assignments are evaluated in order, with each right-hand side evaluated in the scope before
the new variable on the left-hand side has been introduced. Therefore it makes sense to write
something like `let x = x` since the two `x` variables are distinct and have separate storage.
Here is an example where the behavior of `let` is needed:

```jldoctest
julia> Fs = Vector{Any}(undef, 2); i = 1;

julia> while i <= 2
           Fs[i] = ()->i
           global i += 1
       end

julia> Fs[1]()
3

julia> Fs[2]()
3
```

Here we create and store two closures that return variable `i`. However, it is always the same
variable `i`, so the two closures behave identically. We can use `let` to create a new binding
for `i`:

```jldoctest
julia> Fs = Vector{Any}(undef, 2); i = 1;

julia> while i <= 2
           let i = i
               Fs[i] = ()->i
           end
           global i += 1
       end

julia> Fs[1]()
1

julia> Fs[2]()
2
```

Since the `begin` construct does not introduce a new scope, it can be useful to use a zero-argument
`let` to just introduce a new scope block without creating any new bindings:

```jldoctest
julia> let
           local x = 1
           let
               local x = 2
           end
           x
       end
1
```

Since `let` introduces a new scope block, the inner local `x` is a different variable than the
outer local `x`.

### For Loops and Comprehensions

`for` loops, `while` loops, and [Comprehensions](@ref) have the following behavior: any new variables
introduced in their body scopes are freshly allocated for each loop iteration, as if the loop body
were surrounded by a `let` block:

```jldoctest
julia> Fs = Vector{Any}(undef, 2);

julia> for j = 1:2
           Fs[j] = ()->j
       end

julia> Fs[1]()
1

julia> Fs[2]()
2
```

A `for` loop or comprehension iteration variable is always a new variable:

```julia-repl enable_doctest_when_deprecation_warning_is_removed
julia> function f()
           i = 0
           for i = 1:3
           end
           return i
       end;

julia> f()
0
```

However, it is occasionally useful to reuse an existing local variable as the iteration variable.
This can be done conveniently by adding the keyword `outer`:

```jldoctest
julia> function f()
           i = 0
           for outer i = 1:3
           end
           return i
       end;

julia> f()
3
```

## Constants

A common use of variables is giving names to specific, unchanging values. Such variables are only
assigned once. This intent can be conveyed to the compiler using the `const` keyword:

```jldoctest
julia> const e  = 2.71828182845904523536;

julia> const pi = 3.14159265358979323846;
```

Multiple variables can be declared in a single `const` statement:
```jldoctest
julia> const a, b = 1, 2
(1, 2)
```

The `const` declaration should only be used in global scope on globals.
It is difficult for the compiler to optimize code involving global variables, since
their values (or even their types) might change at almost any time. If a global variable will
not change, adding a `const` declaration solves this performance problem.

Local constants are quite different. The compiler is able to determine automatically when a local
variable is constant, so local constant declarations are not necessary, and in fact are currently
not supported.

Special top-level assignments, such as those performed by the `function` and `struct` keywords,
are constant by default.

Note that `const` only affects the variable binding; the variable may be bound to a mutable
object (such as an array), and that object may still be modified. Additionally when one tries
to assign a value to a variable that is declared constant the following scenarios are possible:

* if a new value has a different type than the type of the constant then an error is thrown:
```jldoctest
julia> const x = 1.0
1.0

julia> x = 1
ERROR: invalid redefinition of constant x
```
* if a new value has the same type as the constant then a warning is printed:
```jldoctest
julia> const y = 1.0
1.0

julia> y = 2.0
WARNING: redefining constant y
2.0
```
* if an assignment would not result in the change of variable value no message is given:
```jldoctest
julia> const z = 100
100

julia> z = 100
100
```
The last rule applies for immutable objects even if the variable binding would change, e.g.:
```julia-repl
julia> const s1 = "1"
"1"

julia> s2 = "1"
"1"

julia> pointer.([s1, s2], 1)
2-element Array{Ptr{UInt8},1}:
 Ptr{UInt8} @0x00000000132c9638
 Ptr{UInt8} @0x0000000013dd3d18

julia> s1 = s2
"1"

julia> pointer.([s1, s2], 1)
2-element Array{Ptr{UInt8},1}:
 Ptr{UInt8} @0x0000000013dd3d18
 Ptr{UInt8} @0x0000000013dd3d18
```
However, for mutable objects the warning is printed as expected:
```jldoctest
julia> const a = [1]
1-element Array{Int64,1}:
 1

julia> a = [1]
WARNING: redefining constant a
1-element Array{Int64,1}:
 1
```

Note that although sometimes possible, changing the value of a `const` variable
is strongly discouraged, and is intended only for convenience during
interactive use.
Changing constants can cause various problems or unexpected behaviors.
For instance, if a method references a constant and is already
compiled before the constant is changed then it might keep using the old value:
```jldoctest
julia> const x = 1
1

julia> f() = x
f (generic function with 1 method)

julia> f()
1

julia> x = 2
WARNING: redefining constant x
2

julia> f()
1
```
