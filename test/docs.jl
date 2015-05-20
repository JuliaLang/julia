# This file is a part of Julia. License is MIT: http://julialang.org/license

@doc "Doc abstract type" ->
abstract C74685 <: AbstractArray
@test stringmime("text/plain", Docs.doc(C74685))=="Doc abstract type\n"

macro macro_doctest() end
@doc "Helps test if macros can be documented with `@doc \"...\" -> @...`." ->
@macro_doctest
@test (@doc @macro_doctest) != nothing
