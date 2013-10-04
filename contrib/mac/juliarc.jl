# Set up environment for Julia OSX binary distribution
let
    ROOT = abspath(JULIA_HOME,"..")
    ENV["PATH"]="$JULIA_HOME:$(ENV["PATH"])"
    ENV["FONTCONFIG_PATH"] = joinpath(ROOT, "julia", "etc", "fonts")
    ENV["GIT_EXEC_PATH"] = joinpath(ROOT, "julia", "libexec", "git-core")
    ENV["GIT_TEMPLATE_DIR"] = joinpath(ROOT, "julia", "share", "git-core")
    ENV["TK_LIBRARY"] = "/System/Library/Frameworks/Tk.framework/Versions/8.5/Resources/Scripts"
end
