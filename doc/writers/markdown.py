# -*- coding: utf-8 -*-
"""
    sphinx.writers.markdown
    ~~~~~~~~~~~~~~~~~~~~~~~

    Custom docutils writer for Markdown.

    :copyright: Copyright 2007-2015 by the Sphinx team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""
import os
import re
import textwrap
from itertools import groupby

from six.moves import zip_longest

from docutils import nodes, writers
from docutils.utils import column_width

from sphinx import addnodes
from sphinx.locale import admonitionlabels, _


class TextWrapper(textwrap.TextWrapper):
    """Custom subclass that uses a different word separator regex."""

    wordsep_re = re.compile(
        r'(\s+|'                                  # any whitespace
        r'(?<=\s)(?::[a-z-]+:)?`\S+|'             # interpreted text start
        r'[^\s\w]*\w+[a-zA-Z]-(?=\w+[a-zA-Z])|'   # hyphenated words
        r'(?<=[\w\!\"\'\&\.\,\?])-{2,}(?=\w))')   # em-dash

    def _wrap_chunks(self, chunks):
        """_wrap_chunks(chunks : [string]) -> [string]

        The original _wrap_chunks uses len() to calculate width.
        This method respects wide/fullwidth characters for width adjustment.
        """
        drop_whitespace = getattr(self, 'drop_whitespace', True)  # py25 compat
        lines = []
        if self.width <= 0:
            raise ValueError("invalid width %r (must be > 0)" % self.width)

        chunks.reverse()

        while chunks:
            cur_line = []
            cur_len = 0

            if lines:
                indent = self.subsequent_indent
            else:
                indent = self.initial_indent

            width = self.width - column_width(indent)

            if drop_whitespace and chunks[-1].strip() == '' and lines:
                del chunks[-1]

            while chunks:
                l = column_width(chunks[-1])

                if cur_len + l <= width:
                    cur_line.append(chunks.pop())
                    cur_len += l

                else:
                    break

            if chunks and column_width(chunks[-1]) > width:
                self._handle_long_word(chunks, cur_line, cur_len, width)

            if drop_whitespace and cur_line and cur_line[-1].strip() == '':
                del cur_line[-1]

            if cur_line:
                lines.append(indent + ''.join(cur_line))

        return lines

    def _break_word(self, word, space_left):
        """_break_word(word : string, space_left : int) -> (string, string)

        Break line by unicode width instead of len(word).
        """
        total = 0
        for i, c in enumerate(word):
            total += column_width(c)
            if total > space_left:
                return word[:i-1], word[i-1:]
        return word, ''

    def _split(self, text):
        """_split(text : string) -> [string]

        Override original method that only split by 'wordsep_re'.
        This '_split' split wide-characters into chunk by one character.
        """
        def split(t):
            return textwrap.TextWrapper._split(self, t)
        chunks = []
        for chunk in split(text):
            for w, g in groupby(chunk, column_width):
                if w == 1:
                    chunks.extend(split(''.join(g)))
                else:
                    chunks.extend(list(g))
        return chunks

    def _handle_long_word(self, reversed_chunks, cur_line, cur_len, width):
        """_handle_long_word(chunks : [string],
                             cur_line : [string],
                             cur_len : int, width : int)

        Override original method for using self._break_word() instead of slice.
        """
        space_left = max(width - cur_len, 1)
        if self.break_long_words:
            l, r = self._break_word(reversed_chunks[-1], space_left)
            cur_line.append(l)
            reversed_chunks[-1] = r

        elif not cur_line:
            cur_line.append(reversed_chunks.pop())


MAXWIDTH = 92
STDINDENT = 4


def my_wrap(text, width=MAXWIDTH, **kwargs):
    w = TextWrapper(width=width, **kwargs)
    return w.wrap(text)


class MarkdownWriter(writers.Writer):
    supported = ('markdown',)
    settings_spec = ('No options here.', '', ())
    settings_defaults = {}

    output = None

    def __init__(self, builder):
        writers.Writer.__init__(self)
        self.builder = builder
        self.translator_class = self.builder.translator_class or MarkdownTranslator

    def translate(self):
        visitor = self.translator_class(self.document, self.builder)
        self.document.walkabout(visitor)
        self.output = visitor.body


class MarkdownTranslator(nodes.NodeVisitor):

    def __init__(self, document, builder):
        nodes.NodeVisitor.__init__(self, document)
        self.builder = builder

        newlines = builder.config.text_newlines
        if newlines == 'windows':
            self.nl = '\r\n'
        elif newlines == 'native':
            self.nl = os.linesep
        else:
            self.nl = '\n'
        self.states = [[]]
        self.stateindent = ['']
        self.list_counter = []
        self.sectionlevel = 0
        self.lineblocklevel = 0
        self.table = None

    def add_text(self, text):
        self.states[-1].append((-1, text))

    def new_state(self, indent=STDINDENT):
        self.states.append([])
        if isinstance(indent, int):
            indent = unicode(' ' * indent)
        self.stateindent.append(indent)

    def end_state(self, wrap=True, end=[''], first=None):
        content = self.states.pop()
        maxindent = sum(map(len,self.stateindent))
        indent = self.stateindent.pop()
        result = []
        toformat = []

        def do_format():
            if not toformat:
                return
            if wrap:
                res = my_wrap(''.join(toformat), width=MAXWIDTH-maxindent)
            else:
                res = ''.join(toformat).splitlines()
            if end:
                res += end
            result.append((indent, res))
        for itemindent, item in content:
            if itemindent == -1:
                toformat.append(item)
            else:
                if isinstance(itemindent, int):
                    itemindent = unicode(' ' * itemindent)
                do_format()
                result.append((indent + itemindent, item))
                toformat = []
        do_format()
        if first is not None and result:
            itemindent, item = result[0]
            result_rest, result = result[1:], []
            if item:
                toformat = [first + ' '.join(item)]
                do_format()  # re-create `result` from `toformat`
                _dummy, new_item = result[0]
                result.insert(0, (itemindent[len(indent):], [new_item[0]]))
                result[1] = (itemindent, new_item[1:])
                result.extend(result_rest)
        self.states[-1].extend(result)

    def visit_document(self, node):
        self.new_state(0)
        self.new_state(0)
        self.add_text("---\nlayout: default\n---\n")
        self.end_state(wrap=False)

    def depart_document(self, node):
        self.end_state()
        self.body = self.nl.join((indent + line).rstrip()
                                 for indent, lines in self.states[0]
                                 for line in lines)
        # XXX header/footer?

    def visit_highlightlang(self, node):
        raise nodes.SkipNode

    def visit_section(self, node):
        self.sectionlevel += 1

    def depart_section(self, node):
        self.sectionlevel -= 1

    def visit_sidebar(self, node):
        self.sectionlevel += 1
        self.new_state(0)

    def depart_sidebar(self, node):
        self.end_state()
        self.sectionlevel -= 1

    def visit_rubric(self, node):
        self.visit_section(node)
        self.visit_title(node)

    def depart_rubric(self, node):
        self.depart_title(node)
        self.depart_section(node)

    def visit_compound(self, node):
        pass

    def depart_compound(self, node):
        pass

    def visit_title(self, node):
        self.new_state(0)
        self.add_text('#'*self.sectionlevel + ' ')
        if isinstance(node.parent, nodes.sidebar):
            self.add_text('Sidebar: ')

    def depart_title(self, node):
        self.end_state()

    def visit_desc(self, node):
        pass

    def depart_desc(self, node):
        pass

    def visit_desc_signature(self, node):
        self.new_state(0)
        self.add_text('`')

    def depart_desc_signature(self, node):
        self.add_text('`:')
        self.end_state(wrap=False, end=None)

    def visit_desc_name(self, node):
        pass

    def depart_desc_name(self, node):
        pass

    def visit_desc_addname(self, node):
        pass

    def depart_desc_addname(self, node):
        pass

    def visit_desc_type(self, node):
        pass

    def depart_desc_type(self, node):
        pass

    def visit_desc_returns(self, node):
        self.add_text(' :: ')

    def depart_desc_returns(self, node):
        pass

    def visit_desc_parameterlist(self, node):
        self.add_text('(')
        self.first_param = 1

    def depart_desc_parameterlist(self, node):
        self.add_text(')')

    def visit_desc_parameter(self, node):
        if not self.first_param:
            self.add_text(', ')
        else:
            self.first_param = 0
        self.add_text(node.astext())
        raise nodes.SkipNode

    def visit_desc_optional(self, node):
        self.add_text('[')

    def depart_desc_optional(self, node):
        self.add_text(']')

    def visit_desc_content(self, node):
        self.new_state(2)

    def depart_desc_content(self, node):
        self.end_state()

    def visit_footnote(self, node):
        self._footnote = node.children[0].astext().strip()
        self.new_state(len(self._footnote) + 5)

    def depart_footnote(self, node):
        self.end_state(first='[^%s]: ' % self._footnote)

    def visit_footnote_reference(self, node):
        self.add_text('[^%s]' % node.astext())
        raise nodes.SkipNode

    def visit_citation(self, node):
        if len(node) and isinstance(node[0], nodes.label):
            self._citlabel = '@' + node[0].astext()
        else:
            self._citlabel = ''
        self.new_state(len(self._citlabel) + 4)

    def depart_citation(self, node):
        self.end_state(first='[%s]: ' % self._citlabel)

    def visit_citation_reference(self, node):
        self.add_text('[@%s]' % node.astext())
        raise nodes.SkipNode

    def visit_label(self, node):
        raise nodes.SkipNode

    def visit_tabular_col_spec(self, node):
        raise nodes.SkipNode

    def visit_colspec(self, node):
        self.table[0].append(node['colwidth'])
        raise nodes.SkipNode

    def visit_tgroup(self, node):
        pass

    def depart_tgroup(self, node):
        pass

    def visit_thead(self, node):
        pass

    def depart_thead(self, node):
        pass

    def visit_tbody(self, node):
        self.table.append('sep')

    def depart_tbody(self, node):
        pass

    def visit_row(self, node):
        self.table.append([])

    def depart_row(self, node):
        pass

    def visit_entry(self, node):
        if 'morerows' in node:
            raise NotImplementedError('Row spanning cells are not implemented.')
        self.new_state(0)

    def depart_entry(self, node):
        text = self.nl.join(self.nl.join(x[1]) for x in self.states.pop())
        self.stateindent.pop()
        self.table[-1].append(text)
        if 'morecols' in node:
            n = node['morecols']
            while n > 0:
                self.table[-1].append(u'')
                n = n - 1

    def visit_table(self, node):
        if self.table:
            raise NotImplementedError('Nested tables are not supported.')
        self.new_state(0)
        self.table = [[]]

    def depart_table(self, node):
        lines = self.table[1:]
        fmted_rows = []
        colwidths = self.table[0]
        realwidths = colwidths[:]
        separator = 0
        # don't allow paragraphs in table cells for now
        for line in lines:
            if line == 'sep':
                separator = len(fmted_rows)
            else:
                cells = []
                for i, cell in enumerate(line):
                    par = my_wrap(cell, width=colwidths[i])
                    if par:
                        maxwidth = max(column_width(x) for x in par)
                    else:
                        maxwidth = 0
                    realwidths[i] = max(realwidths[i], maxwidth)
                    cells.append(par)
                fmted_rows.append(cells)

        def writesep(char='-'):
            out = ['|']
            for width in realwidths:
                out.append(char * (width+2))
                out.append('|')
            self.add_text(''.join(out) + self.nl)

        def writerow(row):
            lines = zip_longest(*row)
            for line in lines:
                out = ['|']
                for i, cell in enumerate(line):
                    if cell:
                        adjust_len = len(cell) - column_width(cell)
                        out.append(' ' + cell.ljust(
                            realwidths[i] + 1 + adjust_len))
                    else:
                        out.append(' ' * (realwidths[i] + 2))
                    out.append('|')
                self.add_text(''.join(out) + self.nl)

        for i, row in enumerate(fmted_rows):
            if separator and i == separator:
                writesep('-')
            writerow(row)
        self.table = None
        self.end_state(wrap=False)

    def visit_bullet_list(self, node):
        self.list_counter.append(-1)

    def depart_bullet_list(self, node):
        self.list_counter.pop()

    def visit_enumerated_list(self, node):
        self.list_counter.append(node.get('start', 1) - 1)

    def depart_enumerated_list(self, node):
        self.list_counter.pop()

    def visit_definition_list(self, node):
        self.list_counter.append(-2)

    def depart_definition_list(self, node):
        self.list_counter.pop()

    def visit_list_item(self, node):
        if self.list_counter[-1] == -1:
            # bullet list
            self.new_state(2)
        elif self.list_counter[-1] == -2:
            # definition list
            pass
        else:
            # enumerated list
            self.list_counter[-1] += 1
            self.new_state(len(str(self.list_counter[-1])) + 2)

    def depart_list_item(self, node):
        if self.list_counter[-1] == -1:
            self.end_state(first='* ')
        elif self.list_counter[-1] == -2:
            pass
        else:
            self.end_state(first='%s. ' % self.list_counter[-1])

    def visit_definition_list_item(self, node):
        self._li_has_classifier = len(node) >= 2 and \
            isinstance(node[1], nodes.classifier)

    def depart_definition_list_item(self, node):
        pass

    def visit_term(self, node):
        self.new_state(0)

    def depart_term(self, node):
        if not self._li_has_classifier:
            self.add_text(':')
            self.end_state(end=None)

    def visit_definition(self, node):
        self.new_state(2)

    def depart_definition(self, node):
        self.end_state()

    def visit_field_list(self, node):
        pass

    def depart_field_list(self, node):
        pass

    def visit_field(self, node):
        pass

    def depart_field(self, node):
        pass

    def visit_field_name(self, node):
        self.new_state(0)

    def depart_field_name(self, node):
        self.add_text(':')
        self.end_state(end=None)

    def visit_field_body(self, node):
        self.new_state()

    def depart_field_body(self, node):
        self.end_state()

    def visit_admonition(self, node):
        self.new_state(2)

    def depart_admonition(self, node):
        self.end_state()

    def _visit_admonition(self, node):
        self.new_state(0)

    def _make_depart_admonition(name):
        def depart_admonition(self, node):
            self.end_state(first=admonitionlabels[name] + ': ')
        return depart_admonition

    visit_attention = _visit_admonition
    depart_attention = _make_depart_admonition('attention')
    visit_caution = _visit_admonition
    depart_caution = _make_depart_admonition('caution')
    visit_danger = _visit_admonition
    depart_danger = _make_depart_admonition('danger')
    visit_error = _visit_admonition
    depart_error = _make_depart_admonition('error')
    visit_hint = _visit_admonition
    depart_hint = _make_depart_admonition('hint')
    visit_important = _visit_admonition
    depart_important = _make_depart_admonition('important')
    visit_note = _visit_admonition
    depart_note = _make_depart_admonition('note')
    visit_tip = _visit_admonition
    depart_tip = _make_depart_admonition('tip')
    visit_warning = _visit_admonition
    depart_warning = _make_depart_admonition('warning')
    visit_seealso = _visit_admonition
    depart_seealso = _make_depart_admonition('seealso')

    def visit_literal_block(self, node):
        if node.has_key('language') and node['language'] != 'julia':
            self.new_state(0)
            self.add_text('```' + node['language'] + self.nl)
        else:
            self.new_state()

    def depart_literal_block(self, node):
        if node.has_key('language') and node['language'] != 'julia':
            self.add_text(self.nl + '```')
        self.end_state(wrap=False)

    def visit_doctest_block(self, node):
        self.new_state(0)

    def depart_doctest_block(self, node):
        self.end_state(wrap=False)

    def visit_paragraph(self, node):
        if not isinstance(node.parent, nodes.Admonition) or \
           isinstance(node.parent, addnodes.seealso):
            self.new_state(0)

    def depart_paragraph(self, node):
        if not isinstance(node.parent, nodes.Admonition) or \
           isinstance(node.parent, addnodes.seealso):
            self.end_state()

    def visit_target(self, node):
        raise nodes.SkipNode

    def visit_index(self, node):
        raise nodes.SkipNode

    def visit_substitution_definition(self, node):
        raise nodes.SkipNode

    def visit_compact_paragraph(self, node):
        pass

    def depart_compact_paragraph(self, node):
        pass

    def visit_reference(self, node):
        t = node.astext()
        if t[0] == '[' and t[-1] == ']':
            self.add_text('[@' + t[1:])
            raise nodes.SkipNode
        elif 'refuri' in node and node['refuri'][0:4] == 'http':
            self.add_text('[')

    def depart_reference(self, node):
        if 'refuri' in node and node['refuri'][0:4] == 'http':
            self.add_text('](' + node['refuri'] + ')')

    def visit_emphasis(self, node):
        self.add_text('*')

    def depart_emphasis(self, node):
        self.add_text('*')

    def visit_literal_emphasis(self, node):
        self.add_text('*')

    def depart_literal_emphasis(self, node):
        self.add_text('*')

    def visit_strong(self, node):
        self.add_text('**')

    def depart_strong(self, node):
        self.add_text('**')

    def visit_literal_strong(self, node):
        self.add_text('**')

    def depart_literal_strong(self, node):
        self.add_text('**')

    def visit_title_reference(self, node):
        self.add_text('*')

    def depart_title_reference(self, node):
        self.add_text('*')

    def visit_literal(self, node):
        self.add_text('`')

    def depart_literal(self, node):
        self.add_text('`')

    def visit_subscript(self, node):
        self.add_text('_')

    def depart_subscript(self, node):
        pass

    def visit_superscript(self, node):
        self.add_text('^')

    def depart_superscript(self, node):
        pass

    def visit_Text(self, node):
        self.add_text(node.astext())

    def depart_Text(self, node):
        pass

    def visit_inline(self, node):
        if 'xref' in node['classes'] or 'term' in node['classes']:
            self.add_text('*')

    def depart_inline(self, node):
        if 'xref' in node['classes'] or 'term' in node['classes']:
            self.add_text('*')

    def visit_comment(self, node):
        raise nodes.SkipNode

    def visit_raw(self, node):
        if 'text' in node.get('format', '').split():
            self.body.append(node.astext())
        raise nodes.SkipNode

    def visit_math(self, node):
        self.add_text('``')
        self.add_text(node['latex'])
        self.add_text('``')

    def depart_math(self, node):
        pass

    def visit_displaymath(self, node):
        self.new_state(0)
        self.add_text('```math' + self.nl)
        self.add_text(node['latex'])

    def depart_displaymath(self, node):
        self.add_text(self.nl + '```')
        self.end_state(wrap=False)

    def unknown_visit(self, node):
        raise NotImplementedError('Unknown node: ' + node.__class__.__name__ + '\n' + node.astext())
