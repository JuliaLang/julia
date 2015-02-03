#tests for /base/char.jl

numberchars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
lowerchars = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
upperchars = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']

#char(x::FloatingPoint) = char(round(UInt32,x))
  @test char(1.00000001) == '\x01' #Round down
  @test char(1.49999999) == '\x01' #Round down
  @test char(1.5) == '\x02' #Round Up
  @test char(1.99999999) ==  '\x02' #Round Up

#integer(x::Char) = int(x)
  #tests ASCII 48 - 57
  counter = 48
  for x in numberchars
    @test integer(x) == counter
    counter += 1
  end

  #tests ASCII 65 - 90
  counter = 65
  for x in upperchars
    @test integer(x) == counter
    counter += 1
  end

  #tests ASCII 97 - 122
  counter = 97
  for x in lowerchars
    @test integer(x) == counter
    counter += 1
  end

#convert(::Type{Char}, x::Float16) = char(convert(UInt32, x))
#convert(::Type{Char}, x::Float32) = char(convert(UInt32, x))
#convert(::Type{Char}, x::Float64) = char(convert(UInt32, x))
  @test convert(Char, float16(1)) == convert(Char, float32(1)) == convert(Char, float64(1)) == '\x01'
  @test convert(Char, float16(2)) == convert(Char, float32(2)) == convert(Char, float64(2)) == '\x02'
  @test convert(Char, float16(3)) == convert(Char, float32(3)) == convert(Char, float64(3)) == '\x03'
  @test convert(Char, float16(4)) == convert(Char, float32(4)) == convert(Char, float64(4)) == '\x04'
  @test convert(Char, float16(5)) == convert(Char, float32(5)) == convert(Char, float64(5)) == '\x05'
  @test convert(Char, float16(6)) == convert(Char, float32(6)) == convert(Char, float64(6)) == '\x06'
  @test convert(Char, float16(7)) == convert(Char, float32(7)) == convert(Char, float64(7)) == '\x07'
  @test convert(Char, float16(8)) == convert(Char, float32(8)) == convert(Char, float64(8)) == '\x08'
  @test convert(Char, float16(9)) == convert(Char, float32(9)) == convert(Char, float64(9)) == '\x09'

#size(c::Char) = ()
  for x in upperchars
    @test size(x) == ()
  end

  for x in lowerchars
    @test size(x) == ()
  end

  for x in numberchars
    @test size(x) == ()
  end

#ndims(c::Char) = 0
  for x in upperchars
    @test ndims(x) == 0
  end

  for x in lowerchars
    @test ndims(x) == 0
  end

  for x in numberchars
    @test ndims(x) == 0
  end

#length(c::Char) = 1
  for x in upperchars
    @test length(x) == 1
  end

  for x in lowerchars
    @test length(x) == 1
  end

  for x in numberchars
    @test length(x) == 1
  end

#endof(c::Char) = 1
  for x in upperchars
    @test endof(x) == 1
  end

  for x in lowerchars
    @test endof(x) == 1
  end

  for x in numberchars
    @test endof(x) == 1
  end

#getindex(c::Char) = c
  for x in upperchars
    @test getindex(x) == x
  end

  for x in lowerchars
    @test getindex(x) == x
  end

  for x in numberchars
    @test getindex(x) == x
  end

#first(c::Char) = c
  for x in upperchars
    @test first(x) == x
  end

  for x in lowerchars
    @test first(x) == x
  end

  for x in numberchars
    @test first(x) == x
  end

#last(c::Char) = c
  for x in upperchars
    @test last(x) == x
  end

  for x in lowerchars
    @test last(x) == x
  end

  for x in numberchars
    @test last(x) == x
  end

#eltype(c::Char) = Char
  for x in upperchars
    @test eltype(x) == Char
  end

  for x in lowerchars
    @test eltype(x) == Char
  end

  for x in numberchars
    @test eltype(x) == Char
  end

#start(c::Char) = false
  for x in upperchars
    @test start(x) == false
  end

  for x in lowerchars
    @test start(x) == false
  end

  for x in numberchars
    @test start(x) == false
  end

#next(c::Char, state) = (c, true)
  for x in upperchars
    for state in [true, false]
      @test next(x, state) == (x, true)
    end
  end

  for x in lowerchars
    for state in [true, false]
      @test next(x, state) == (x, true)
    end
  end

  for x in numberchars
    for state in [true, false]
      @test next(x, state) == (x, true)
    end
  end

#done(c::Char, state) = state
  for x in upperchars
    for state in [true, false]
      @test done(x, state) == state
    end
  end

  for x in lowerchars
    for state in [true, false]
      @test done(x, state) == state
    end
  end

  for x in numberchars
    for state in [true, false]
      @test done(x, state) == state
    end
  end

#isless(x::Char, y::Integer) = isless(uint32(x), y)
  for x in upperchars
    @test isless(x, 91) == true
  end

  for x in lowerchars
    @test isless(x, 123) == true
  end

  for x in numberchars
    @test isless(x, 66) == true
  end

#isless(x::Integer, y::Char) = isless(x, uint32(y))
  for x in upperchars
    @test isless(64, x) == true
  end

  for x in lowerchars
    @test isless(96, x) == true
  end

  for x in numberchars
    @test isless(47, x) == true
  end

