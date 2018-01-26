# This file is a part of Julia. License is MIT: https://julialang.org/license

# Set up environment for Julia OSX binary distribution
let
    ROOT = abspath(Sys.BINDIR,"..")
    ENV["PATH"]="$(Sys.BINDIR):$(ENV["PATH"])"
    ENV["FONTCONFIG_PATH"] = joinpath(ROOT, "etc", "fonts")
    ENV["TK_LIBRARY"] = "/System/Library/Frameworks/Tk.framework/Versions/8.5/Resources/Scripts"
end
