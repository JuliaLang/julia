* Vector{UInt8}(s::StringView{Vector{UInt8}}) now copies
* Remove `StringView(::String)` - semantically bad
* Remove IOBuffer constructor
* Require one based indexing, which the code assumes
* Use Base's optimised isascii
* Use Base's new optimised hashing
* s[1:2] was type unstable, and also returned a view. Now slices the underlying vector
* No SVRegexMatch, just uses RegexMatchq
* Remove `reverse`, because opt-in becomes breaking

* Remove some inference tests that makes no sense


# TODO:
* Decide if we can use inbounds, given that the underlying array is generic


This PR ports [StringViews.jl](https://github.com/JuliaStrings/StringViews.jl) to Base, as discussed in #60037.

## Motivation
We generally want to avoid adding more types to Base when it can live in a package, so this PR requires some justification:

### StringViews are the more fundamental string
`String` and `SubString` are an abstraction over an underlying byte array, and almost all its operations are defined in terms of loading bytes from that array. This abstraction of strings as "arrays in a trench coat" is made explicit by string views.
Hence, `String` and `SubString` can be implemented in terms of string views, but not the other way around. And it is better if Base contains the foundations and packages provide implementations on top of those, instead of the other way around.

One hint that the relationship is inverted is to look at the implementation of `StringViews.jl`: It re-implements tonnes of internal Base string functions, instead of calling into generic, foundational methods in Base.
We also have comments like this in Base: `# duck-type s so that external UTF-8 string packages like StringViews can hook in`, where the sensible thing would be to _define_ getindex for UTF8-encoded strings in terms of this method, instead of encouraging use of Base internals.

Due to the strong intersection of strings as an abstraction and string views, this PR only has ~100 LOC in `base/strings/stringview.jl`, whereas `src/` in `StringViews.jl` has ~800 LOC.

Relatedly, I find it unfortunate that this relationship is frequently inverted for `String` in Base - code will often define operations on `CodeUnits{UInt8, String}` in terms of operations on the string, instead of the other way around. This makes the 'dispatch funnels' awkward because it does not match the fundamental truth that string operations are ultimately defined (even in Base) in terms of byte operations.

### StringViews could be used in Base itself
Base does a bunch of parsing, e.g. on TOML and Tar files. In my view, string views are very useful for zero-copy operations for efficient parsing. For example, to parse a `Float32` from a byte buffer, we now jave to allocate an intermediate `String`. If we ever want to optimise Tar and TOML parsing, we would need something like string views at some point.

### Arguments against this PR:
Stefan mentioned that he an Oscar was discussing some string redesign that would lessen the need for string views. I don't know the details, but would like to know more. If string views are made obsolete in the future, it may be a bad idea to add this type.

This PR also does complicate some dispatch systems. Since [we don't have an interface for expressing denseness](https://github.com/JuliaLang/julia/issues/54581), this PR worsens the situation where dispatch is controlled by "big unions" that attempt to encompass the same concept, such as e.g. `DenseString = Union{SubString{String}, String, StringView{<:Union{DenseVector{UInt8}, var"#s18"} where var"#s18"<:(SubArray{UInt8, 1, var"#s16", I, true} where {var"#s16"<:DenseVector{UInt8}, I<:Union{Tuple{Vararg{Real}}, Tuple{AbstractUnitRange, Vararg{Any}}}})}, SubString{<:StringView{<:Union{DenseVector{UInt8}, var"#s18"} where var"#s18"<:(SubArray{UInt8, 1, var"#s16", I, true} where {var"#s16"<:DenseVector{UInt8}, I<:Union{Tuple{Vararg{Real}}, Tuple{AbstractUnitRange, Vararg{Any}}}})}}}`.
This problem is possibly temporary, and could be solved, but it does mean that the existence of string views in Base makes existing Base code more complex, and therefore increases the risk of bugs.

## Differences from StringViews.jl to this PR
The `StringView` type in this PR is different from that in StringViews.jl in several respects, listed below. The purpose of the changes I've made is to make the implementation more 'conservative', such that the semantics of string views are more in line with existing Base code.

I have also removed a bunch of auxiliary methods that are slighly off, semantically, mixing up strings, vectors and IOs.

* `Vector{UInt8}(s::StringView{Vector{UInt8}})` now copies, instead of accessing the wrapped vector, because it is confusing that the `Vector{UInt8}(::StringView)` constructor sometimes and sometimes not aliases the string. The `codeunits` function can be used to unambiguously get a vector that aliases the string.
* The constructor `StringView(::String)` has been removed. String views are - by definition - built from `AbstractVector{UInt8}`, not strings. If you for (some weird) reason wants a string view wrapping a string, do `StringView(codeunits(::String))`
* The `StringView(buf::IOBuffer)` constructor has been removed. The purpose of this function was to wrap the buffer of `IOBuffer`. However, this should be done properly with a function that gets the buffer of `IOBuffer` as an `AbstractVector{UInt8}`, then wrapping that in a string view.
* Slicing into a string view, e.g. `s[1:2]` now copies, instead of returning a view. It has also been type-stabilized.
* `SVRegexMatch` has been removed. Regex matching over a string view just returns a `RegexMatch`. This is objectively better, but was not done in StringViews.jl because `RegexMatch` used to not be generic over the target string.
* `reverse(::StringView)` no longer returns a `String`, because this fallback would make any specialization breaking (as it would no longer return a `String`)
* For now, `StringView` can only be constructed with a one-based indexed vector. This restriction may be lifted in the future. This check has been added because some existing StringView code assumed that and I didn't want to re-invent StringViews.jl too much in this PR.
* It is not permitted to construct a `StringView{T1}` wrapping a `T2` where `T2 <: T1` and `T1 !== T2`. This just complicates the code for no good reason.

## Other discussion points with this PR
* For now, this PR has removed a few inference tests in Base. From what I can tell, these tests relied on world splitting for correctness, which is not proper. I'll investigate closer before merging.
* Can we use `@inbounds` with a `StringView` wrapping an unknown array type? The code currently does, but I'm not against removing it. If we do remove it, we may need to re-jigger a bunch of string related code, since shared code paths of strings and string views would mean that `String` performance could be degraded by removing inbounds annotations.
