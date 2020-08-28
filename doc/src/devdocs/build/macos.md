# Building on macOS

You need to have the current Xcode command line utilities installed: run `xcode-select --install` in the terminal. You will need to rerun this terminal command after each macOS update, otherwise you may run into errors involving missing libraries or headers.

The dependent libraries are now built with [BinaryBuilder](https://binarybuilder.org) and will be automatically downloaded. This is the preferred way to build Julia source. In case you want to build them all on your own, you will need a 64-bit gfortran to compile Julia dependencies.
```bash
brew install gcc
```

If you have set `LD_LIBRARY_PATH` or `DYLD_LIBRARY_PATH` in your `.bashrc` or equivalent, Julia may be unable to find various libraries that come bundled with it. These environment variables need to be unset for Julia to work.
