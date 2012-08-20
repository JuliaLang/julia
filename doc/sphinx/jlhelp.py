import codecs
from os import path

from docutils import nodes
from sphinx.builders.text import TextBuilder
from sphinx.writers.text import TextTranslator
from sphinx.writers.text import TextWriter

from sphinx.util.osutil import ensuredir
from sphinx.util.console import bold, purple, darkgreen, term_width_line
from pudb import set_trace; debug_here = set_trace

def jl_escape(text):
    # XXX: crude & fragile
    return text.replace('"',"'").replace('\\',r'\\')

class JuliaHelpTranslator(TextTranslator):

    def __init__(self, document, builder):
        TextTranslator.__init__(self, document, builder)
        self.in_desc = False

    def add_text(self, text, escape=True, force=False):
        if self.in_desc or force:
            etext = jl_escape(text) if escape else text
            self.states[-1].append((-1, etext))

    def visit_title(self, node):
        if self.sectionlevel == 1:
            self._current_title = node.astext()
        raise nodes.SkipNode

    def visit_desc(self, node):
        if node.attributes['objtype'] == 'attribute':
            return
        self.in_desc = True
        self.new_state(0)

    def visit_desc_name(self, node):
        self._desc_name = node.astext()
        TextTranslator.visit_desc_name(self, node)

    def depart_desc(self, node):
        if node.attributes['objtype'] == 'attribute':
            return
        self.add_text('"}\n', escape=False)
        self.end_state(first='{E"%s"  E"%s"  E"' % ( \
            jl_escape(self._current_title), \
            jl_escape(self._desc_name)))
        self.in_desc = False

class JuliaHelpWriter(TextWriter):

    def translate(self):
        visitor = JuliaHelpTranslator(self.document, self.builder)
        self.document.walkabout(visitor)
        self.output = visitor.body

class JuliaHelpBuilder(TextBuilder):
    name = "jlhelp"
    out_suffix = ".jl"

    def write(self, *ignored):
        # build_all
        docnames = set([doc for doc in self.env.found_docs if doc.startswith("stdlib")])

        self.info(bold('preparing documents... '), nonl=True)
        self.prepare_writing(docnames)
        self.info('done')

        # write target files
        warnings = []
        self.env.set_warnfunc(lambda *args: warnings.append(args))

        outfilename = path.join(self.outdir, self.name + self.out_suffix)
        ensuredir(path.dirname(outfilename))
        try:
            f = codecs.open(outfilename, 'w', 'utf-8')
            try:
                f.write('# automatically generated -- do not edit\n\n' +
                        '[\n\n')

                for docname in self.status_iterator(
                    sorted(docnames), 'processing... ', darkgreen, len(docnames)):
                    doctree = self.env.get_and_resolve_doctree(docname, self)
                    self.writer.write(doctree, f)
                    f.write("\n")

                f.write('\n]\n')
            finally:
                f.close()
        except (IOError, OSError), err:
            self.warn("error writing file %s: %s" % (outfilename, err))

        for warning in warnings:
            self.warn(*warning)
        self.env.set_warnfunc(self.warn)

    def prepare_writing(self, docnames):
        self.writer = JuliaHelpWriter(self)

def setup(app):
    app.add_builder(JuliaHelpBuilder)
