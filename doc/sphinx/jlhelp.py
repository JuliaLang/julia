
from docutils import nodes
from sphinx.builders.text import TextBuilder
from sphinx.writers.text import TextTranslator
from sphinx.writers.text import TextWriter

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
        self.in_desc = True
        self.new_state(0)

    def visit_desc_name(self, node):
        self._desc_name = node.astext()
        TextTranslator.visit_desc_name(self, node)

    def depart_desc(self, node):
        self.add_text('"),\n', escape=False)
        self.end_state(first='(E"%s",E"%s",E"' % ( \
            jl_escape(self._current_title), \
            jl_escape(self._desc_name)))
        self.in_desc = False

class JuliaHelpWriter(TextWriter):

    def translate(self):
        visitor = JuliaHelpTranslator(self.document, self.builder)
        self.document.walkabout(visitor)
        self.output = '# automatically generated -- do not edit\n\n' \
            + 'function _jl_help_db() return [\n\n' \
            + visitor.body \
            + '\n] end\n'

class JuliaHelpBuilder(TextBuilder):
    name = "jlhelp"
    out_suffix = ".jl"

    def prepare_writing(self, docnames):
        self.writer = JuliaHelpWriter(self)

def setup(app):
    app.add_builder(JuliaHelpBuilder)

