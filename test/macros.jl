# Test some macro behavior that is not tested elsewhere

# The complex @printf macro is thoroughly tested elsewhere

# Test proper hygiene in the complex @which macro
# @less, @edit, @code_typed, @code_lowered, @code_llvm, @code_native are
# defined in the same way as @which so they don't need independent testing

module Which_test
  type UnexpectedException <: Exception
    which::Int
  end
  function Base.showerror(io::IO, e::UnexpectedException)
    print(io, "should not be called ", e.which)
  end

  # Error if hygiene fails and the wrong function is called
  typesof(x...) = throw(UnexpectedException(1))
  error(x...)   = throw(UnexpectedException(2))
  which(x...)   = throw(UnexpectedException(3))

  f1(x::Int)  = "int"
  f1(x::Real) = "real"

  function test1(n)
    # Error if hygiene fails and the wrong function is called
    typesof(x...) = throw(UnexpectedException(4))
    error(x...)   = throw(UnexpectedException(5))
    which(x...)   = throw(UnexpectedException(6))

    return @which(f1(n)).sig
  end

  f2(x::Int; k=1)  = "int"
  f2(x::Real; k=2) = "real"

  function test2(n)
    typesof(x...) = throw(UnexpectedException(7))
    error(x...)   = throw(UnexpectedException(8))
    which(x...)   = throw(UnexpectedException(9))

    return @which(f2(n, k=5)).sig
  end

  f3(x::Int, y::Real...) = x .+ y

  function test3(nx, ny)
    typesof(x...) = throw(UnexpectedException(10))
    error(x...)   = throw(UnexpectedException(11))
    which(x...)   = throw(UnexpectedException(12))

    return @which(f3(nx, ny...)).sig
  end

  function test4(a, i)
    typesof(x...) = throw(UnexpectedException(13))
    error(x...)   = throw(UnexpectedException(14))
    which(x...)   = throw(UnexpectedException(15))

    # Have to make a string because type parameters in the method signature
    # are output but cannot be input
    return string(@which(a[i]).sig)
  end

  function test5(a, i)
    typesof(x...) = throw(UnexpectedException(16))
    error(x...)   = throw(UnexpectedException(17))
    which(x...)   = throw(UnexpectedException(18))

    return string((@which a[i]).sig)
  end

  function test6(a, i, x)
    typesof(x...) = throw(UnexpectedException(19))
    error(x...)   = throw(UnexpectedException(20))
    which(x...)   = throw(UnexpectedException(21))

    return string(@which(a[i] = x).sig)
  end

  function test7(a, i, x)
    typesof(x...) = throw(UnexpectedException(22))
    error(x...)   = throw(UnexpectedException(23))
    which(x...)   = throw(UnexpectedException(24))

    return string((@which a[i] = x).sig)
  end
end

# Test the simplest and most common case first
@test Which_test.test1(1.0) == (Real,)
@test Which_test.test1(123) == (Int,)

# Should get error("no method found for the specified argument types")
# in which(), not UnexpectedException
@test_throws ErrorException Which_test.test1("foo")

# Test gen_call_with_extracted_types's alternate path for keyword arguments
@test Which_test.test2(1.0) == (Real,)
@test Which_test.test2(123) == (Int,)
 
# Test gen_call_with_extracted_types's alternate path for splatted arguments
@test Which_test.test3(1, {1.0, 2.0}) == (Int, Real...)

# Test gen_call_with_extracted_types's alternate path for subscripting
t4 = (1, 2, 3, 4)
a4 = {1, 2, 3, 4}
i1 = 1
x7 = 7
@test Which_test.test4(t4, i1) == "((Any...,),$Int)"
@test Which_test.test5(t4, i1) == "((Any...,),$Int)"
@test Which_test.test4(a4, i1) == "(Array{T,N},Real)"   # yes, Real.  Why?
@test Which_test.test5(a4, i1) == "(Array{T,N},Real)"
@test Which_test.test6(a4, i1, x7) == "(Array{Any,N},Any,Real)"
@test Which_test.test7(a4, i1, x7) == "(Array{Any,N},Any,Real)"
