The `contrib/` directory has support files for using Julia with various third-party software.
Currently, there is support for the Emacs, Vim, and TextMate editors, which can be setup by following the instructions in the following sections.

<a name="Emacs"/>
### Emacs

Add the following line to `~/.emacs`

    (require 'julia-mode "JULIA_PATH/contrib/julia-mode.el")

where `JULIA_PATH` is the location of the top-level julia directory.

<a name="Vim"/>
### Vim

Instructions for installing Vim support for Julia can be found in the [julia-vim repo](https://github.com/JuliaLang/julia-vim), which provides pathogen, vbundle, and manual installation options.

<a name="TextMate"/>
### TextMate

Copy (or symlink) the TextMate Julia bundle into the TextMate application support directory:

    cp -r JULIA_PATH/contrib/Julia.tmbundle ~/Library/Application\ Support/TextMate/Bundles/

where `JULIA_PATH` is the location of the top-level julia directory.
Now select from the menu in TextMate `Bundles > Bundle Editor > Reload Bundles`.
Julia should appear as a file type and be automatically detected for files with the `.j` extension.
