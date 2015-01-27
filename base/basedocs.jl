import .Docs: keywords

keywords[:hello] = keywords[:hi] = doc"Hello, Human."

const intro = doc"""
  **Welcome to Julia $(string(VERSION)).** The full manual is available at

      http://docs.julialang.org/

  as well many great tutorials and learning resources:

      http://julialang.org/learning/

  For help on a specific function or macro, type `?` followed
  by its name, e.g. `?fft`, `?@time` or `?html""`, and press
  enter.

  You can also use `apropos("...")` to search the documentation.
  """

keywords[:help] = keywords[:?] = keywords[:julia] = keywords[:wtf] = intro

keywords[:using] = doc"""
  `using` will load the given module or package and make some of its names
  available for use (see also `export`). For example:

      using Gadfly

  loads the plotting package, Gadfly, so that the `plot` function can be used.

  Names can be used via dot syntax, whether they are exported or not:

      Gadfly.plot(...)

  If you don't want to use the packages exports directly, see also `import`.
  If you're not sure, `using` is almost definitely what you want.
  """

keywords[:import] = doc"""
      import Gadfly

  `import`, like `using`, will load modules and packages for use. Unlike
  `using`, however, it will *not* make any `export`ed names available for use.
  To use Gadfly's `plot` function after importing it, for example, you have
  to write:

      Gadfly.plot(...)

  Import can also be used with specific names, for example

      import Gadfly: plot, render

  This syntax is used when you want to extend the modules functions with
  new methods.
  """

keywords[:export] = doc"""
  `export` is used within modules and packages to tell Julia which functions
  should be made available to the user. For example:

      module Test
      export foo # foo is exported, but bar isn't
      foo(x) = x
      bar(y) = y
      end

      using Test
      foo(1) # 1
      bar(1) # Error: bar not defined
      Test.bar(1) # 1
  """

keywords[:const] = doc"""
  `const` is used to declare global variables which are also constant.
  In almost all code (and particularly performance sensitive code)
  global variables should be declared constant in this way.

      const x = 5

  Note that "constant-ness" is not enforced inside containers, so if
  `x` is an array or dictionary (for example) you can still add and remove
  elements.

  Technically, you can even redefine `const` variables, although this will
  generate a warning from the compiler. The only strict requirement is that
  the *type* of the variable does not change, which is why `const` variables
  are much faster than regular globals.
  """

keywords[:function] = doc"""
  Functions are defined with the `function` keyword:

      function add(a, b)
        return a + b
      end

  Or the short form notation:

      add(a, b) = a + b

  The use of the `return` keyword is exactly the same as in other languages,
  but is often optional. When it's not used, the last expression in the function
  body will be returned by default:

      function compare(a, b)
        a == b && return "equal to"
        a < b ? "less than" : "greater than"
      end
  """

keywords[:return] = doc"""
  `return` can be used function bodies to exit early and return a given value,
  e.g.

      function compare(a, b)
        a == b && return "equal to"
        a < b ? "less than" : "greater than"
      end

  In general you can place a `return` statement anywhere within a function
  body, including within deeply nested loops or conditionals, but be careful
  with `do` blocks. For example:

      function test1(xs)
        for x in xs
          iseven(x) && return 2x
        end
      end

      function test2(xs)
        map(xs) do x
          iseven(x) && return 2x
          x
        end
      end

  In the first example, the return breaks out of its enclosing function
  as soon as it hits an even number, so `test1([5,6,7])` returns `12`.

  You might expect the second example to behave the same way, but in fact
  the `return` there only breaks out of the *inner* function (inside the `do`
  block) and gives a value back to `map`. `test2([5,6,7])` then returns `[5,12,7]`.
  """

keywords[:break] = doc"""
  `break` breaks out of a loop immediately. For example

      i = 0
      while true
        i += 1
        i > 10 && break
        println(i)
      end

  prints the numbers 1 to 10.
  """

keywords[:continue] = doc"""
  `continue` skips the rest of the current loop, then carries on
  looping. For example

      for i = 1:10
        iseven(i) && continue
        println(i)
      end

  prints the numbers 1, 3, 5..., skipping the even numbers.
  """

keywords[:do] = doc"""
  The `do` keyword creates an anonymous function. For example

      map(1:10) do x
        2x
      end

  is equivalent to `map(x->2x, 1:10)`.

  Use multiple arguments like so:

      map(1:10, 10:20) do x, y
        x + y
      end

  Splats (`...`) are not supported in do block notation yet.
  """

keywords[:...] = doc"""
  The "splat" operator, `...`, represents a sequence of arguments. For example

      add(xs...) = reduce(+, xs)

  can take any number of arguments:

      add(1, 2, 3, 4, 5)

  `...` can also be used to apply a function to a sequence of arguments like so:

      add([1, 2, 3]...) # 6
      add(7, 1:100..., 1000:1100...) # 111107
  """

@doc doc"""
  Executes an expression, printing the time it took to
  execute and the total number of bytes its execution caused to be
  allocated. Returns the value of the expression. For example:

      @time begin
        sleep(1)
        2+2
      end
  """ @time

@doc doc"""
  Construct a regex, such as `r"^[a-z]*$"`. The regex also accepts
  one or more flags, listed after the ending quote, to change its
  behaviour:

    • `i` enables case-insensitive matching
    • `m` treats the `^` and `$` tokens as matching the start and
      and end of individual lines, as opposed to the whole string.
    • `s` allows the `.` modifier to match newlines.
    • `x` enables "comment mode": whitespace is enabled except when
      escaped with `\`, and `#` is treated as starting a comment.

  For example, this regex has all three flags enabled:

      julia> match(r"a+.*b+.*?d$"ism, "Goodbye,\nOh, angry,\nBad world\n")
      RegexMatch("angry,\nBad world")
  """ r""

@doc (@doc r"") @r_mstr

@doc doc"""
      push!(collection, items...) → collection

  Insert `items` at the end of `collection`.

      push!([1,2,3], 4) == [1,2,3,4]
  """ push!

@doc doc"""
      fft(A[, dims])

  Performs a multidimensional FFT of the array `A`.  The optional
  `dims` argument specifies an iterable subset of dimensions (e.g.
  an integer, range, tuple, or array) to transform along.  Most
  efficient if the size of `A` along the transformed dimensions is
  a product of small primes; see `nextprod()`.  See also
  `plan_fft()` for even greater efficiency.

  A one-dimensional FFT computes the one-dimensional discrete Fourier
  transform (DFT) as defined by

  $$\operatorname{DFT}(A)[k] =
    \sum_{n=1}^{\operatorname{length}(A)}
    \exp\left(-i\frac{2\pi
    (n-1)(k-1)}{\operatorname{length}(A)} \right) A[n].$$

  A multidimensional FFT simply performs this operation along each
  transformed dimension of `A`.

  Higher performance is usually possible with multi-threading. Use
  `FFTW.set_num_threads(np)` to use `np` threads, if you have `np`
  processors.
  """ fft

@doc doc"""
      include("file.jl")

  Evaluate the contents of a source file in the current context.
  During including, a task-local include path is set to the directory
  containing the file. Nested calls to `include` will search
  relative to that path. All paths refer to files on node 1 when
  running in parallel, and files will be fetched from node 1. This
  function is typically used to load source interactively, or to
  combine files in packages that are broken into multiple source
  files.
  """ include_from_node1
