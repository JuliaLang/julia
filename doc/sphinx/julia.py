# Julia domain for Sphinx
# http://sphinx.pocoo.org/domains.html

from sphinx.domains.python import PythonDomain

class JuliaDomain(PythonDomain):
    """Julia language domain."""
    name = 'jl'
    label = 'Julia'

def setup(app):
    app.add_domain(JuliaDomain)

