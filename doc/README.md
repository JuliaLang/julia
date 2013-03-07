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

    $ make helpdb_en.jl
    $ make html
    $ make latexpdf


File layout
-----------

    conf.py             Sphinx configuration
    helpdb_en.jl           REPL help database
    sphinx/             Sphinx extensions and plugins
    sphinx/jlhelp.py    Sphinx plugin to build helpdb_en.jl
    stdlib/             Julia standard library documentation
    _themes/            Sphinx html themes

