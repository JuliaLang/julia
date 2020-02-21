# This file is a part of Julia. License is MIT: https://julialang.org/license

mktempdir() do dir

  # Create test file
  filename = joinpath(dir, "file.txt")
  text = "123456"
  write(filename, text)

  # test filesystem truncate (shorten)
  file = Base.Filesystem.open(filename, Base.Filesystem.JL_O_RDWR)
  Base.Filesystem.truncate(file, 2)
  @test length(read(file)) == 2
  close(file)

  # test filesystem truncate (lengthen)
  file = Base.Filesystem.open(filename, Base.Filesystem.JL_O_RDWR)
  Base.Filesystem.truncate(file, 20)
  @test length(read(file)) == 20
  close(file)

  # test filesystem futime
  file = Base.Filesystem.open(filename, Base.Filesystem.JL_O_RDWR)
  Base.Filesystem.futime(file, 1.0, 1.0)
  close(file)

  # test filesystem readbytes!
  file = Base.Filesystem.open(filename, Base.Filesystem.JL_O_RDWR)
  res = zeros(UInt8,20*4)
  Base.Filesystem.readbytes!(file, res)
  res[1] == 0x31
  close(file)

end
