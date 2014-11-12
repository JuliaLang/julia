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
