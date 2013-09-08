if OS_NAME == :Darwin
    if isfile(JULIA_HOME*"/git")
        ENV["PATH"] = JULIA_HOME*":"*ENV["PATH"]
        ENV["GIT_EXEC_PATH"] = JULIA_HOME*"/../../julia/libexec/git-core"
        ENV["GIT_TEMPLATE_DIR"] = JULIA_HOME*"/../../julia/share/git-core"
    end

    ENV["TK_LIBRARY"] = "/System/Library/Frameworks/Tk.framework/Versions/8.5/Resources/Scripts"
end
