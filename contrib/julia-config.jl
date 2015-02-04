const options =
[
   "--cflags",
   "--ldflags"
];

function imagePath()
   opts = Base.compileropts();
   image_path = bytestring(opts.image_file);
end

function libDir()
   @windows_only dir = joinpath(Base.JULIA_HOME, "julia");
   @unix_only dir = match(r"(.*)(sys.ji)",imagePath()).captures[1];
end

function includeDir()
   dir = match(r"(.*)(bin)",JULIA_HOME).captures[1]*"include/julia";
end

function initDir()
   @unix_only dir = match(r"(.*)(julia/sys.ji)",imagePath()).captures[1];
end

function ldflags()
   @unix_only "-L$(libDir()) -Wl,-rpath $(libDir()) -ljulia";
end

function cflags()
   @unix_only "-DJULIA_INIT_DIR=\"$(initDir())\" -I$(includeDir())";
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
