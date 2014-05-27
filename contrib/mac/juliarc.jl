# Set up environment for Julia OSX binary distribution
let
    ROOT = abspath(JULIA_HOME,"..")
    ENV["PATH"]="$JULIA_HOME:$(joinpath(ROOT, "libexec")):$(ENV["PATH"])"
    ENV["FONTCONFIG_PATH"] = joinpath(ROOT, "etc", "fonts")
    ENV["TK_LIBRARY"] = "/System/Library/Frameworks/Tk.framework/Versions/8.5/Resources/Scripts"
end
