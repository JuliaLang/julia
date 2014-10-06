Julia Documentation README
==========================

Julia's documentation is written in reStructuredText, a good reference for which
is the [Documenting Python](http://docs.python.org/devguide/documenting.html)
chapter of the Python Developer's Guide.


Prerequisites for building the documentation
--------------------------------------------

The documentation is built using [Sphinx](http://sphinx.pocoo.org/) and LaTeX.
On ubuntu, you'll need the following packages installed:

    latex-cjk-all
    texlive
    texlive-lang-cjk
    texlive-latex-extra

On OS X, you can install install MacTex using the GUI installer


Building the documentation
--------------------------

Build the documentation by running

    $ make helpdb.jl
    $ make html
    $ make latexpdf


File layout
-----------

    conf.py             Sphinx configuration
    helpdb.jl           REPL help database
    stdlib/             Julia standard library documentation
    UNDOCUMENTED.rst    Undocumented functions (to be filled in and copied to 
                        the correct location in stdlib/)

Sphinx extensions and theme
---------------------------
The extensions to Sphinx and the theme are in the
https://github.com/JuliaLang/JuliaDoc repository, and can also be used to style
package documentation.
