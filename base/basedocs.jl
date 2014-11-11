@doc doc"""
      @time ex
  Executes the expression `ex`, printing the time it took to
  execute and the total number of bytes its execution caused to be
  allocated. Returns the value of the expression. For example:

      @time begin
        sleep(1)
        2+2
      end
  """ @time

@doc doc"""
      r"..."
  Construct a regex, such as `r"^[a-z]*$"`.
  """ r""

@doc doc"""
      push!(collection, items...) → collection

  Insert `items` at the end of `collection`.
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
