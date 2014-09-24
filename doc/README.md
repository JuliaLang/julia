Julia Documentation README
==========================

Julia's documentation is written in reStructuredText, a good reference for which
is the [Documenting Python](http://docs.python.org/devguide/documenting.html)
chapter of the Python Developer's Guide.


Prerequisites for building the documentation
--------------------------------------------

The documentation is built using [Sphinx](http://sphinx.pocoo.org/) and LaTeX.
On ubuntu, you'll need the following packages installed:

    python-sphinx
    python-pip
    latex-cjk-all
    texlive
    texlive-lang-cjk
    texlive-latex-extra

Use pip to install sphinx_rtd_theme:

    $ sudo pip install sphinx_rtd_theme


On OS X, you can install these packages with [homebrew](http://brew.sh/) and [MacTex](https://tug.org/mactex/)

    brew install python
    brew install sphinx
	(install MacTex using the GUI installer)

and install sphinx_rtd_theme as for Ubuntu.


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
