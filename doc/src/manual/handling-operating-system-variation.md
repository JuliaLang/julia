# Handling Operating System Variation

When dealing with platform libraries, it is often necessary to provide special cases for various
platforms. The variable `Sys.KERNEL` can be used to write these special cases. There are several
functions in the `Sys` module intended to make this easier: `isunix`, `islinux`, `isapple`,
`isbsd`, and `iswindows`. These may be used as follows:

```julia
if Sys.iswindows()
    some_complicated_thing(a)
end
```

Note that `islinux` and `isapple` are mutually exclusive subsets of `isunix`. Additionally,
there is a macro `@static` which makes it possible to use these functions to conditionally hide
invalid code, as demonstrated in the following examples.

Simple blocks:

```
ccall((@static Sys.iswindows() ? :_fopen : :fopen), ...)
```

Complex blocks:

```julia
@static if Sys.islinux()
    some_complicated_thing(a)
else
    some_different_thing(a)
end
```

When chaining conditionals (including `if`/`elseif`/`end`), the `@static` must be repeated for
each level (parentheses optional, but recommended for readability):

```julia
@static Sys.iswindows() ? :a : (@static Sys.isapple() ? :b : :c)
```
