:mod:`sound.jl` --- Functions for audio
=======================================================

.. module::sound.jl
   :synopsis: Functions for acoustic processing

This module contains functions intended to process acoustic samples.

RIFF/WAVE Functions
-------------------
These functions are used for loading and saving RIFF/WAVE (wav) functions. The API is very similar to the one found in MATLAB.

Here is a quick example that copies an existing file:

::

    julia> require("sound.jl")

    julia> import Sound.*

    julia> in_filename = ARGS[1]

    julia> y, Fs, nbits, extra = wavread(in_filename)

    julia> wavwrite(y, Fs, nbits, strcat("out-", in_filename))

.. note:: This implementation only supports little endian machines right now.

.. function:: Sound.wavread(io [, options])

   Reads and returns the samples from a RIFF/WAVE file. The samples are converted to floating
   point values in the range from -1.0 to 1.0 by default. The ``io`` argument accepts either an
   ``IO`` object or a filename (``String``). The options are passed via an ``Options`` object
   (see the :ref:`options page <options-module>`).

   The available options, and the default values, are:

   * ``format`` (default = ``double``): changes the format of the returned samples. The string
     ``double`` returns double precision floating point values in the range -1.0 to 1.0. The string
     ``native`` returns the values as encoded in the file. The string ``size`` returns the number
     of samples in the file, rather than the actual samples.
   * ``subrange`` (default = ``Any``): controls which samples are returned. The default, ``Any``
     returns all of the samples. Passing a number (``Real``), ``N``, will return the first ``N``
     samples of each channel. Passing a range (``Range1{Real}``), ``R``, will return the samples
     in that range of each channel.

   The returned values are:

   * ``y``: The acoustic samples; A matrix is returned for files that contain multiple channels.
   * ``Fs``: The sampling frequency
   * ``nbits``: The number of bits used to encode each sample
   * ``extra``: Any additional bytes used to encode the samples (is always ``None``)

   The following functions are also defined to make this function compatible with MATLAB:

::

   wavread(filename::String) = wavread(filename, @options)
   wavread(filename::String, fmt::String) = wavread(filename, @options format=fmt)
   wavread(filename::String, N::Int) = wavread(filename, @options subrange=N)
   wavread(filename::String, N::Range1{Int}) = wavread(filename, @options subrange=N)
   wavread(filename::String, N::Int, fmt::String) = wavread(filename, @options subrange=N format=fmt)
   wavread(filename::String, N::Range1{Int}, fmt::String) = wavread(filename, @options subrange=N format=fmt)
   
.. function:: Sound.wavwrite(samples, io [, options])

    Writes samples to a RIFF/WAVE file io object. The ``io`` argument
    accepts either an ``IO`` object or a filename (``String``). The
    function assumes that the sample rate is 8 kHz and uses 16 bits to
    encode each sample. Both of these values can be changed with the
    options parameter. Each column of the data represents a different
    channel. Stereo files should contain two columns. The options are
    passed via an ``Options`` object (see the :ref:`options page
    <options-module>`).

    The available options, and the default values, are:

   * ``sample_rate`` (default = ``8000``): sampling frequency
   * ``nbits`` (default = ``16``): number of bits used to encode each
     sample

   The type of the input array, samples, also affects the generated
   file. "Native" WAVE files are written when integers are passed into
   wavwrite. This means that the literal values are written into the
   file. The input ranges are as follows for integer samples.

   ======       ===========     ======================   =============
   N Bits       y Data Type     y Data Range             Output Format
   ======       ===========     ======================   =============
   8            uint8           0 <= y <= 255            uint8
   16           int16           –32768 <= y <= +32767    int16
   24           int32           –2^23 <= y <= 2^23 – 1   int32
   ======       ===========     ======================   =============

   If samples contains floating point values, the input data ranges
   are the following.

   ======    ================   =================   =============
   N Bits    y Data Type        y Data Range        Output Format
   ======    ================   =================   =============
   8         single or double   –1.0 <= y < +1.0    uint8
   16        single or double   –1.0 <= y < +1.0    int16
   24        single or double   –1.0 <= y < +1.0    int32
   32        single or double   –1.0 <= y <= +1.0   single
   ======    ================   =================   =============

   The following functions are also defined to make this function
   compatible with MATLAB:
::

    wavwrite(y::Array) = wavwrite(y, @options)
    wavwrite(y::Array, Fs::Real, filename::String) = wavwrite(y, filename, @options sample_rate=Fs)
    wavwrite(y::Array, Fs::Real, N::Real, filename::String) = wavwrite(y, filename, @options sample_rate=Fs nbits=N)
