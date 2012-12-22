# Julia domain for Sphinx
# http://sphinx.pocoo.org/domains.html

import re
import sphinx.domains.python

sphinx.domains.python.py_sig_re = re.compile(
    r'''^ ([\w.]*\.)?            # class name(s)
          ([^\s(]+)  \s*         # thing name
          (?: \((.*)\)           # optional: arguments
           (?:\s* -> \s* (.*))?  #           return annotation
          )? $                   # and nothing more
          ''', re.VERBOSE | re.UNICODE)

class JuliaDomain(sphinx.domains.python.PythonDomain):
    """Julia language domain."""
    name = 'jl'
    label = 'Julia'

JuliaDomain.directives['type'] = JuliaDomain.directives['class']

def setup(app):
    app.add_domain(JuliaDomain)

