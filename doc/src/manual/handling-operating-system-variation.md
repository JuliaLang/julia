# Handling Operating System Variation

When dealing with platform libraries, it is often necessary to provide special cases for various
platforms. The variable `Sys.KERNEL` can be used to write these special cases. There are several
functions intended to make this easier: `is_unix`, `is_linux`, `is_apple`, `is_bsd`, and `is_windows`.
These may be used as follows:

```julia
if is_windows()
    some_complicated_thing(a)
end
```

Note that `is_linux` and `is_apple` are mutually exclusive subsets of `is_unix`. Additionally,
there is a macro `@static` which makes it possible to use these functions to conditionally hide
invalid code, as demonstrated in the following examples.

Simple blocks:

```
ccall( (@static is_windows() ? :_fopen : :fopen), ...)
```

Complex blocks:

```julia
@static if is_linux()
    some_complicated_thing(a)
else
    some_different_thing(a)
end
```

When chaining conditionals (including if/elseif/end), the `@static` must be repeated for each
level (parentheses optional, but recommended for readability):

```julia
@static is_windows() ? :a : (@static is_apple() ? :b : :c)
```
