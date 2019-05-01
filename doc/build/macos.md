## macOS

You need to have the current Xcode command line utilities installed: run `xcode-select --install` in the terminal.
You will need to rerun this terminal command after each macOS update, otherwise you may run into errors involving missing libraries or headers.
You will also need a 64-bit gfortran to compile Julia dependencies. The gfortran-4.7 (and newer) compilers in Homebrew work for building Julia.

If you have set `LD_LIBRARY_PATH` or `DYLD_LIBRARY_PATH` in your `.bashrc` or equivalent, Julia may be unable to find various libraries that come bundled with it. These environment variables need to be unset for Julia to work.
