# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Filesystem: StatStruct, samefile

show(DevNull, StatStruct())
mktempdir() do d
    mktemp(d) do f, io
        @test typeof(stat(RawFD(fd(io)))) <: StatStruct
        @test typeof(stat(fd(io))) <: StatStruct
        @test typeof(stat(splitdir(f)...)) <:StatStruct
        @test typeof(lstat(splitdir(f)...)) <: StatStruct
        @test ispath(d)
        @test !ispath(joinpath(d, randstring(17)))
        if is_unix()
            ff = joinpath(d, string(randstring(17), "_fifo"))
            run(`mkfifo $ff`)
            @test isfifo(ff)
            @test !isfifo(f)
            @test ischardev("/dev/stderr")
            @test !ischardev(d)
        end
        @test isdir(d)
        @test !isdir(f)
        # More portable suggestions are welcome.
        if is_linux()
            @test isblockdev("/dev/ram0")
            @test !isblockdev("/")
        end
        @test isfile(f)
        @test !isfile(d)
        if is_unix()
            fl = string(f, "_symlink")
            symlink(f, fl)
            @test !islink(f)
            @test islink(fl)
            @test samefile(f, fl)
            mktemp(d) do f′, _
                @test !samefile(f′, f)
                @test !samefile(f′, fl)
            end
        end
        @test samefile(f, f)
        @test samefile(d, d)
        @test !samefile(f, d)
        if is_unix()
            sf = joinpath(d, string(randstring(17), "_sock"))
            s  = listen(sf)
            try
                @test issocket(sf)
                @test !issocket(f)
            finally
                close(s)
            end
        end
        if is_unix()
            run(`chmod u+s $f`)
            @test issetuid(f)
            run(`chmod u-s $f`)
            @test !issetuid(f)
            run(`chmod g+s $f`)
            @test issetgid(f)
            run(`chmod g-s $f`)
            @test !issetgid(f)
            run(`chmod o+t $d`)
            @test issticky(d)
            run(`chmod o-t $d`)
            @test !issticky(d)
        end
        @test uperm(f) != 0
        if !is_windows()
            @test gperm(f) == 0
            @test operm(f) == 0
        else
            @test gperm(f) != 0
            @test operm(f) != 0
        end
        @test filemode(f) == stat(f).mode
        @test filesize(f) == 0
        # In practice, these two should always be true.
        @test mtime(f) > 0
        @test ctime(f) > 0
        if !is_windows()
            m = mtime(f)
            # Is `touch` a NOP on Windows or just unreliable?
            touch(f)
            @test m < mtime(f)
        end
        @test ismount(is_windows() ? "C:\\" : "/")
        @test !ismount(f)
    end
end
