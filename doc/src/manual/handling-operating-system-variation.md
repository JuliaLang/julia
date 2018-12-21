# Handling Operating System Variation

When writing cross-platform applications or libraries, it is often necessary to allow for
differences between operating systems. The variable `Sys.KERNEL` can be used to handle such
cases. There are several functions in the `Sys` module intended to make this easier, such as
`isunix`, `islinux`, `isapple`, `isbsd`, `isfreebsd`, and `iswindows`. These may be used
as follows:

```julia
if Sys.iswindows()
    windows_specific_thing(a)
end
```

Note that `islinux`, `isapple`, and `isfreebsd` are mutually exclusive subsets of `isunix`.
Additionally, there is a macro `@static` which makes it possible to use these functions to
conditionally hide invalid code, as demonstrated in the following examples.

Simple blocks:

```
ccall((@static Sys.iswindows() ? :_fopen : :fopen), ...)
```

Complex blocks:

```julia
@static if Sys.islinux()
    linux_specific_thing(a)
else
    generic_thing(a)
end
```

When chaining conditionals (including `if`/`elseif`/`end`), the `@static` must be repeated for
each level (parentheses optional, but recommended for readability):

```julia
@static Sys.iswindows() ? :a : (@static Sys.isapple() ? :b : :c)
```

