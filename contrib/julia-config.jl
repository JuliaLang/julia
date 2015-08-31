#!/usr/bin/env julia
# This file is a part of Julia. License is MIT: http://julialang.org/license

const options =
[
    "--cflags",
    "--ldflags",
    "--ldlibs"
];

function imagePath()
    opts = Base.JLOptions();
    bytestring(opts.image_file);
end

function libDir()
    abspath(dirname(Libdl.dlpath("libjulia")));
end

function includeDir()
    joinpath(match(r"(.*)(bin)",JULIA_HOME).captures[1],"include","julia");
end

function unixInitDir()
    filePart = split(imagePath(),"/")[end]
    return match(Regex("(.*)(/julia/$filePart)"),imagePath()).captures[1];
end

function windowsInitDir()
    if imagePath()[end-1:end] == "ji"
        return match(r"(.*)(\\julia\\sys.ji)",imagePath()).captures[1];
    else
        return match(r"(.*)(\\julia\\sys.dll)",imagePath()).captures[1];
    end
end

function initDir()
    @unix_only return unixInitDir();
    @windows_only return windowsInitDir();
end

function ldflags()
    replace("""-L$(libDir())""","\\","\\\\");
end

function ldlibs()
    @unix_only return replace("""-Wl,-rpath,$(libDir()) -ljulia""","\\","\\\\");
    @windows_only return replace("""-ljulia""","\\","\\\\");
end

function cflags()
    arg1 = replace(initDir(),"\\","\\\\\\\\");
    arg2 = replace(includeDir(),"\\","\\\\");
    return """-DJULIA_INIT_DIR=\\"$arg1\\" -I$arg2""";
end

function check_args(args)
    checked = intersect(args,options);
    if length(checked) == 0 || length(checked) != length(args)
        println(STDERR,"Usage: julia-config [",reduce((x,y)->"$x|$y",options),"]");
        exit(1);
    end
end

function main()
    check_args(ARGS);
    for args in ARGS
        if args == "--ldflags"
            println(ldflags());
        elseif args == "--cflags"
            println(cflags());
        elseif args == "--ldlibs"
            println(ldlibs());
        end
    end
end

main();
