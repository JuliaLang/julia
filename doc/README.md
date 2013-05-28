Julia Documentation README
==========================

Julia's documentation is written in reStructuredText, a good reference for which
is the [Documenting Python](http://docs.python.org/devguide/documenting.html)
chapter of the Python Developer's Guide.


Building the documentation
--------------------------

The documentation is built using [Sphinx](http://sphinx.pocoo.org/) and LaTeX.
On ubuntu, you'll need the following packages installed:

    python-sphinx
    texlive
    texlive-latex-extra

Then run

    $ make helpdb.jl
    $ make html
    $ make latexpdf


File layout
-----------

    conf.py             Sphinx configuration
    helpdb.jl           REPL help database
    stdlib/             Julia standard library documentation

Sphinx extensions and theme
---------------------------
The extensions to Sphinx and the theme are in the
https://github.com/JuliaLang/JuliaDoc repository, and can also be used to style
package documentation.
