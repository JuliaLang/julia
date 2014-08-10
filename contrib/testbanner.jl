# Check for existence of version file telling us the last version we tested
if isinteractive()
    vers = Base.get_firstboot_version()
    if vers == nothing || vers < Base.VERSION
        print("It looks like this version of Julia is new on this machine. ")
        print("Please run the testsuite to ensure proper setup by typing ")
        print_with_color(:red, "Base.runtests() ")
        print("at the repl, or silence this message for this version of julia by typing ")
        print_with_color(:red, "Base.silence_test_banner()\n")
    end
end
