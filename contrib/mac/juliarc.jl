# This file is a part of Julia. License is MIT: http://julialang.org/license

# Set up environment for Julia OSX binary distribution
let
    ROOT = abspath(JULIA_HOME,"..")
    ENV["PATH"]="$JULIA_HOME:$(joinpath(ROOT, "libexec", "git-core")):$(ENV["PATH"])"
    ENV["FONTCONFIG_PATH"] = joinpath(ROOT, "etc", "fonts")
    ENV["GIT_EXEC_PATH"] = joinpath(ROOT, "libexec", "git-core")
    ENV["GIT_TEMPLATE_DIR"] = joinpath(ROOT, "share", "git-core")
    ENV["TK_LIBRARY"] = "/System/Library/Frameworks/Tk.framework/Versions/8.5/Resources/Scripts"
end
