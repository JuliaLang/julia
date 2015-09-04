# Makefile for Sphinx documentation

default: html

# You can set these variables from the command line.
SPHINXOPTS       =
PAPER            =
JULIAHOME        = $(abspath ..)
JULIA_EXECUTABLE = $(JULIAHOME)/usr/bin/julia

# Internal variables.
PAPEROPT_a4     = -D latex_paper_size=a4
PAPEROPT_letter = -D latex_paper_size=letter
ALLSPHINXOPTS   = -d _build/doctrees $(PAPEROPT_$(PAPER)) $(SPHINXOPTS) .
# the i18n builder cannot share the environment and doctrees with the others
I18NSPHINXOPTS  = $(PAPEROPT_$(PAPER)) $(SPHINXOPTS) .

JULIA_ENV     = $(JULIAHOME)/deps/julia-env
ACTIVATE      = $(JULIA_ENV)/bin/activate
SPHINX_BUILD  = $(JULIA_ENV)/bin/sphinx-build

$(ACTIVATE):
	$(MAKE) -C $(JULIAHOME)/deps install-virtualenv
	touch -c $@

$(SPHINX_BUILD): $(ACTIVATE) requirements.txt
	. $(ACTIVATE) && pip install sphinx==1.3.1 \
	              && pip install -r requirements.txt
	touch -c $@

SPHINXBUILD = . $(ACTIVATE) && sphinx-build

.PHONY: help clean cleanall html dirhtml singlehtml pickle json htmlhelp qthelp devhelp \
	epub latex latexpdf text man changes linkcheck doctest gettext

help:
	@echo "Please use 'make <target>' where <target> is one of"
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
	-rm -rf _build/*

cleanall: clean

html: $(SPHINX_BUILD)
	$(SPHINXBUILD) -b html $(ALLSPHINXOPTS) _build/html
	@echo
	@echo "Build finished. The HTML pages are in _build/html."

dirhtml: $(SPHINX_BUILD)
	$(SPHINXBUILD) -b dirhtml $(ALLSPHINXOPTS) _build/dirhtml
	@echo
	@echo "Build finished. The HTML pages are in _build/dirhtml."

singlehtml: $(SPHINX_BUILD)
	$(SPHINXBUILD) -b singlehtml $(ALLSPHINXOPTS) _build/singlehtml
	@echo
	@echo "Build finished. The HTML page is in _build/singlehtml."

pickle: $(SPHINX_BUILD)
	$(SPHINXBUILD) -b pickle $(ALLSPHINXOPTS) _build/pickle
	@echo
	@echo "Build finished; now you can process the pickle files."

json: $(SPHINX_BUILD)
	$(SPHINXBUILD) -b json $(ALLSPHINXOPTS) _build/json
	@echo
	@echo "Build finished; now you can process the JSON files."

htmlhelp: $(SPHINX_BUILD)
	$(SPHINXBUILD) -b htmlhelp $(ALLSPHINXOPTS) _build/htmlhelp
	@echo
	@echo "Build finished; now you can run HTML Help Workshop with the" \
	      ".hhp project file in _build/htmlhelp."

qthelp: $(SPHINX_BUILD)
	$(SPHINXBUILD) -b qthelp $(ALLSPHINXOPTS) _build/qthelp
	@echo
	@echo "Build finished; now you can run "qcollectiongenerator" with the" \
	      ".qhcp project file in _build/qthelp, like this:"
	@echo "# qcollectiongenerator _build/qthelp/JuliaLanguage.qhcp"
	@echo "To view the help file:"
	@echo "# assistant -collectionFile _build/qthelp/JuliaLanguage.qhc"

devhelp: $(SPHINX_BUILD)
	$(SPHINXBUILD) -b devhelp $(ALLSPHINXOPTS) _build/devhelp
	@echo
	@echo "Build finished."
	@echo "To view the help file:"
	@echo "# mkdir -p $$HOME/.local/share/devhelp/JuliaLanguage"
	@echo "# ln -s _build/devhelp $$HOME/.local/share/devhelp/JuliaLanguage"
	@echo "# devhelp"

epub: $(SPHINX_BUILD)
	$(SPHINXBUILD) -b epub $(ALLSPHINXOPTS) _build/epub
	@echo
	@echo "Build finished. The epub file is in _build/epub."

latex: $(SPHINX_BUILD)
	$(SPHINXBUILD) -b latex $(ALLSPHINXOPTS) _build/latex
	@echo
	@echo "Build finished; the LaTeX files are in _build/latex."
	@echo "Run 'make' in that directory to run these through (pdf)latex" \
	      "(use 'make latexpdf' here to do that automatically)."

latexpdf: $(SPHINX_BUILD)
	$(SPHINXBUILD) -b latex $(ALLSPHINXOPTS) _build/latex
	@echo "Running LaTeX files through pdflatex..."
	$(MAKE) -C _build/latex all-pdf
	@echo "pdflatex finished; the PDF files are in _build/latex."

text: $(SPHINX_BUILD)
	$(SPHINXBUILD) -b text $(ALLSPHINXOPTS) _build/text
	@echo
	@echo "Build finished. The text files are in _build/text."

man: $(SPHINX_BUILD)
	$(SPHINXBUILD) -b man $(ALLSPHINXOPTS) _build/man
	@echo
	@echo "Build finished. The manual pages are in _build/man."

texinfo: $(SPHINX_BUILD)
	$(SPHINXBUILD) -b texinfo $(ALLSPHINXOPTS) _build/texinfo
	@echo
	@echo "Build finished. The Texinfo files are in _build/texinfo."
	@echo "Run 'make' in that directory to run these through makeinfo" \
	      "(use 'make info' here to do that automatically)."

info: $(SPHINX_BUILD)
	$(SPHINXBUILD) -b texinfo $(ALLSPHINXOPTS) _build/texinfo
	@echo "Running Texinfo files through makeinfo..."
	make -C _build/texinfo info
	@echo "makeinfo finished; the Info files are in _build/texinfo."

gettext: $(SPHINX_BUILD)
	$(SPHINXBUILD) -b gettext $(I18NSPHINXOPTS) _build/locale
	@echo
	@echo "Build finished. The message catalogs are in _build/locale."

changes: $(SPHINX_BUILD)
	$(SPHINXBUILD) -b changes $(ALLSPHINXOPTS) _build/changes
	@echo
	@echo "The overview file is in _build/changes."

linkcheck: $(SPHINX_BUILD)
	$(SPHINXBUILD) -b linkcheck $(ALLSPHINXOPTS) _build/linkcheck
	@echo
	@echo "Link check complete; look for any errors in the above output " \
	      "or in _build/linkcheck/output.txt."

doctest: $(SPHINX_BUILD)
	PATH="$(PATH):$(build_bindir)" $(SPHINXBUILD) -b doctest $(ALLSPHINXOPTS) _build/doctest
	@echo "Testing of doctests in the sources finished, look at the " \
	      "results in _build/doctest/output.txt."

manual/unicode-input-table.rst: $(JULIAHOME)/base/latex_symbols.jl
	$(JULIA_EXECUTABLE) tabcomplete.jl > manual/unicode-input-table.rst

unicode: manual/unicode-input-table.rst

