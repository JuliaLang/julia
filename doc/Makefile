# Makefile for Sphinx documentation
#

# You can set these variables from the command line.
SPHINXOPTS    =
PAPER         =
BUILDDIR      = _build
JULIAHOME     = $(abspath ..)

# Internal variables.
PAPEROPT_a4     = -D latex_paper_size=a4
PAPEROPT_letter = -D latex_paper_size=letter
ALLSPHINXOPTS   = -d $(BUILDDIR)/doctrees $(PAPEROPT_$(PAPER)) $(SPHINXOPTS) .
# the i18n builder cannot share the environment and doctrees with the others
I18NSPHINXOPTS  = $(PAPEROPT_$(PAPER)) $(SPHINXOPTS) .

JULIA_ENV=$(JULIAHOME)/deps/julia-env
$(JULIA_ENV)/bin/activate:
	$(MAKE) -C $(JULIAHOME)/deps install-virtualenv
	touch -c $@

SPHINX_BUILD=$(JULIA_ENV)/bin/sphinx-build
$(SPHINX_BUILD): $(JULIA_ENV)/bin/activate
	. $(JULIA_ENV)/bin/activate && \
	  pip install sphinx==1.1.3
	touch -c $@

SPHINX_RTD_THEME=$(JULIA_ENV)/lib/python2.7/site-packages/sphinx_rtd_theme
$(SPHINX_RTD_THEME): $(SPHINX_BUILD)
	. $(JULIA_ENV)/bin/activate && \
	  pip install sphinx_rtd_theme
	touch -c $@

SPHINXBUILD = . $(JULIA_ENV)/bin/activate && env PYTHONPATH=juliadoc sphinx-build

.PHONY: help clean cleanall html dirhtml singlehtml pickle json htmlhelp qthelp devhelp \
	epub latex latexpdf text man changes linkcheck doctest gettext

help:
	@echo "Please use 'make <target>' where <target> is one of"
	@echo "  helpdb.jl  to make the REPL help db"
	@echo "  html       to make standalone HTML files"
	@echo "  dirhtml    to make HTML files named index.html in directories"
	@echo "  singlehtml to make a single large HTML file"
	@echo "  pickle     to make pickle files"
	@echo "  json       to make JSON files"
	@echo "  htmlhelp   to make HTML files and a HTML help project"
	@echo "  qthelp     to make HTML files and a qthelp project"
	@echo "  devhelp    to make HTML files and a Devhelp project"
	@echo "  epub       to make an epub"
	@echo "  latex      to make LaTeX files, you can set PAPER=a4 or PAPER=letter"
	@echo "  latexpdf   to make LaTeX files and run them through pdflatex"
	@echo "  text       to make text files"
	@echo "  man        to make manual pages"
	@echo "  texinfo    to make Texinfo files"
	@echo "  info       to make Texinfo files and run them through makeinfo"
	@echo "  gettext    to make PO message catalogs"
	@echo "  changes    to make an overview of all changed/added/deprecated items"
	@echo "  linkcheck  to check all external links for integrity"
	@echo "  doctest    to run all doctests embedded in the documentation (if enabled)"

clean:
	-rm -rf $(BUILDDIR)/*
	# We also need to remove .pyc files
	-find . -name *.pyc -delete

cleanall: clean

juliadoc-pkg: juliadoc/juliadoc/__init__.py
	@-cd .. && git submodule update doc/juliadoc

html: juliadoc-pkg $(SPHINX_RTD_THEME)
	$(SPHINXBUILD) -b html $(ALLSPHINXOPTS) $(BUILDDIR)/html
	@echo
	@echo "Build finished. The HTML pages are in $(BUILDDIR)/html."

dirhtml: juliadoc-pkg $(SPHINX_RTD_THEME)
	$(SPHINXBUILD) -b dirhtml $(ALLSPHINXOPTS) $(BUILDDIR)/dirhtml
	@echo
	@echo "Build finished. The HTML pages are in $(BUILDDIR)/dirhtml."

singlehtml: juliadoc-pkg $(SPHINX_RTD_THEME)
	$(SPHINXBUILD) -b singlehtml $(ALLSPHINXOPTS) $(BUILDDIR)/singlehtml
	@echo
	@echo "Build finished. The HTML page is in $(BUILDDIR)/singlehtml."

pickle: juliadoc-pkg $(SPHINX_RTD_THEME)
	$(SPHINXBUILD) -b pickle $(ALLSPHINXOPTS) $(BUILDDIR)/pickle
	@echo
	@echo "Build finished; now you can process the pickle files."

json: juliadoc-pkg $(SPHINX_RTD_THEME)
	$(SPHINXBUILD) -b json $(ALLSPHINXOPTS) $(BUILDDIR)/json
	@echo
	@echo "Build finished; now you can process the JSON files."

htmlhelp: juliadoc-pkg $(SPHINX_RTD_THEME)
	$(SPHINXBUILD) -b htmlhelp $(ALLSPHINXOPTS) $(BUILDDIR)/htmlhelp
	@echo
	@echo "Build finished; now you can run HTML Help Workshop with the" \
	      ".hhp project file in $(BUILDDIR)/htmlhelp."

qthelp: juliadoc-pkg $(SPHINX_RTD_THEME)
	$(SPHINXBUILD) -b qthelp $(ALLSPHINXOPTS) $(BUILDDIR)/qthelp
	@echo
	@echo "Build finished; now you can run "qcollectiongenerator" with the" \
	      ".qhcp project file in $(BUILDDIR)/qthelp, like this:"
	@echo "# qcollectiongenerator $(BUILDDIR)/qthelp/JuliaLanguage.qhcp"
	@echo "To view the help file:"
	@echo "# assistant -collectionFile $(BUILDDIR)/qthelp/JuliaLanguage.qhc"

devhelp: juliadoc-pkg $(SPHINX_RTD_THEME)
	$(SPHINXBUILD) -b devhelp $(ALLSPHINXOPTS) $(BUILDDIR)/devhelp
	@echo
	@echo "Build finished."
	@echo "To view the help file:"
	@echo "# mkdir -p $$HOME/.local/share/devhelp/JuliaLanguage"
	@echo "# ln -s $(BUILDDIR)/devhelp $$HOME/.local/share/devhelp/JuliaLanguage"
	@echo "# devhelp"

epub: juliadoc-pkg $(SPHINX_RTD_THEME)
	$(SPHINXBUILD) -b epub $(ALLSPHINXOPTS) $(BUILDDIR)/epub
	@echo
	@echo "Build finished. The epub file is in $(BUILDDIR)/epub."

latex: juliadoc-pkg $(SPHINX_RTD_THEME)
	$(SPHINXBUILD) -b latex $(ALLSPHINXOPTS) $(BUILDDIR)/latex
	@echo
	@echo "Build finished; the LaTeX files are in $(BUILDDIR)/latex."
	@echo "Run 'make' in that directory to run these through (pdf)latex" \
	      "(use 'make latexpdf' here to do that automatically)."

latexpdf: juliadoc-pkg $(SPHINX_RTD_THEME)
	$(SPHINXBUILD) -b latex $(ALLSPHINXOPTS) $(BUILDDIR)/latex
	@echo "Running LaTeX files through pdflatex..."
	$(MAKE) -C $(BUILDDIR)/latex all-pdf
	@echo "pdflatex finished; the PDF files are in $(BUILDDIR)/latex."

text: juliadoc-pkg $(SPHINX_RTD_THEME)
	$(SPHINXBUILD) -b text $(ALLSPHINXOPTS) $(BUILDDIR)/text
	@echo
	@echo "Build finished. The text files are in $(BUILDDIR)/text."

man: juliadoc-pkg $(SPHINX_RTD_THEME)
	$(SPHINXBUILD) -b man $(ALLSPHINXOPTS) $(BUILDDIR)/man
	@echo
	@echo "Build finished. The manual pages are in $(BUILDDIR)/man."

texinfo: juliadoc-pkg $(SPHINX_RTD_THEME)
	$(SPHINXBUILD) -b texinfo $(ALLSPHINXOPTS) $(BUILDDIR)/texinfo
	@echo
	@echo "Build finished. The Texinfo files are in $(BUILDDIR)/texinfo."
	@echo "Run 'make' in that directory to run these through makeinfo" \
	      "(use 'make info' here to do that automatically)."

info: juliadoc-pkg $(SPHINX_RTD_THEME)
	$(SPHINXBUILD) -b texinfo $(ALLSPHINXOPTS) $(BUILDDIR)/texinfo
	@echo "Running Texinfo files through makeinfo..."
	make -C $(BUILDDIR)/texinfo info
	@echo "makeinfo finished; the Info files are in $(BUILDDIR)/texinfo."

gettext: juliadoc-pkg $(SPHINX_RTD_THEME)
	$(SPHINXBUILD) -b gettext $(I18NSPHINXOPTS) $(BUILDDIR)/locale
	@echo
	@echo "Build finished. The message catalogs are in $(BUILDDIR)/locale."

changes: juliadoc-pkg $(SPHINX_RTD_THEME)
	$(SPHINXBUILD) -b changes $(ALLSPHINXOPTS) $(BUILDDIR)/changes
	@echo
	@echo "The overview file is in $(BUILDDIR)/changes."

linkcheck: juliadoc-pkg $(SPHINX_RTD_THEME)
	$(SPHINXBUILD) -b linkcheck $(ALLSPHINXOPTS) $(BUILDDIR)/linkcheck
	@echo
	@echo "Link check complete; look for any errors in the above output " \
	      "or in $(BUILDDIR)/linkcheck/output.txt."

doctest: juliadoc-pkg $(SPHINX_RTD_THEME)
	PATH=$(PATH):$(build_bindir) $(SPHINXBUILD) -b doctest $(ALLSPHINXOPTS) $(BUILDDIR)/doctest
	@echo "Testing of doctests in the sources finished, look at the " \
	      "results in $(BUILDDIR)/doctest/output.txt."

helpdb.jl: stdlib/*.rst juliadoc-pkg $(SPHINX_RTD_THEME)
	$(SPHINX_BUILD) -b jlhelp $(ALLSPHINXOPTS) $(BUILDDIR)/jlhelp
	mv $(BUILDDIR)/jlhelp/jlhelp.jl helpdb.jl
