const options =
[
    "--cflags",
    "--ldflags"
];

function imagePath()
    opts = Base.compileropts();
    bytestring(opts.image_file);
end

function libDir()
    @unix_only return match(r"(.*)(sys.ji)",imagePath()).captures[1];
    @windows_only return Base.JULIA_HOME;
end

function includeDir()
    joinpath(match(r"(.*)(bin)",JULIA_HOME).captures[1],"include","julia");
end

function initDir()
    @unix_only return match(r"(.*)(julia/sys.ji)",imagePath()).captures[1];
    @windows_only return match(r"(.*)(julia\sys.ji)",imagePath()).captures[1];
end

function ldflags()
    @unix_only return "-L$(libDir()) -Wl,-rpath $(libDir()) -ljulia";
    @windows_only return "-L$(libDir()) -ljulia";
end

function cflags()
    "-DJULIA_INIT_DIR=\"$(initDir())\" -I$(includeDir())";
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
        end
    end
end

main();
