#!/usr/bin/env julia

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
    abspath(dirname(Sys.dlpath("libjulia")));
end

function includeDir()
    joinpath(match(r"(.*)(bin)",JULIA_HOME).captures[1],"include","julia");
end

function initDir()
    @unix_only return match(r"(.*)(/julia/sys.ji)",imagePath()).captures[1];
    @windows_only return match(r"(.*)(\\julia\\sys.ji)",imagePath()).captures[1];
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
