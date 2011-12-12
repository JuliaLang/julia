<a name="Emacs-Setup"/>
## Emacs Setup

Add the following line to `~/.emacs`

    (require 'julia-mode "JULIA_PATH/contrib/julia-mode.el")

where `JULIA_PATH` is the location of the top-level julia directory.

<a name="TextMate-Setup"/>
## TextMate Setup

Copy (or symlink) the TextMate Julia bundle into the TextMate application support directory:

    cp -r JULIA_PATH/contrib/Julia.tmbundle ~/Library/Application\ Support/TextMate/Bundles/

where `JULIA_PATH` is the location of the top-level julia directory.
Now select from the menu in TextMate `Bundles > Bundle Editor > Reload Bundles`.
Julia should appear as a file type and be automatically detected for files with the `.j` extension.

<a name="Vim-Setup"/>
## ViM Setup

Copy (or symlink) the vim Julia subdirectories contents into the vim application support directory:

    cp -r JULIA_PATH/contrib/vim/* ~/.vim

where `JULIA_PATH` is the location of the top-level julia directory.
Julia should appear as a file type and be automatically detected for files with the `.j` extension.
