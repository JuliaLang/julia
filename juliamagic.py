"""Julia magics for IPython.
"""

#-----------------------------------------------------------------------------
#  Copyright (C) 2013 The IPython Development Team
#
#  Distributed under the terms of the BSD License.  The full license is in
#  the file COPYING, distributed as part of this software.
#-----------------------------------------------------------------------------

# Global to ensure we don't try to initialize the Julia interpreter more than
# once, so this can actually be reloaded.

import ctypes
import ctypes.util
import sys
import commands

from IPython.core.magic import ( Magics, magics_class,
                                 line_cell_magic )


class JuliaMagicError(Exception):
    pass

###########################################################################
# Julia magics using Julia PyCall module to perform type conversions

@magics_class
class JuliaMagics(Magics):
    """A set of magics useful for interactive work with Julia.
    """
    def __init__(self, shell):
        """
        Parameters
        ----------
        shell : IPython shell

        """

        global _julia_initialized
        
        super(JuliaMagics, self).__init__(shell)

        # Ugly hack to register the julia interpreter globally so we can reload
        # this extension without trying to re-open the shared lib, which kills
        # the python interpreter.  Nasty but useful while debugging
        if hasattr(sys, '_julia_initialized'):
            j = sys._julia_initialized
            self._j = j
            return
        
        print 'Finding Julia install directory...'
        status, JULIA_HOME = commands.getstatusoutput('julia -e "print(JULIA_HOME)"')
        if status != 0:
            raise JuliaMagicError("error executing julia command")
        j = ctypes.PyDLL(ctypes.util.find_library('%s/../lib/libjulia-release' % JULIA_HOME), ctypes.RTLD_GLOBAL)
        print 'Initializing Julia...'
        j.jl_init('%s/../lib' % JULIA_HOME)
        sys._julia_initialized = j

        self._j = j
        j.jl_eval_string.restype = ctypes.c_void_p
        j.jl_call1.restype = ctypes.c_void_p
        j.jl_get_field.restype = ctypes.c_void_p
        j.jl_typeof_str.restype = ctypes.c_char_p
        j.jl_unbox_voidpointer.restype = ctypes.py_object
        
        print 'Initializing Julia PyCall module...'
        self.jcall('using PyCall')
        self.jcall('pyinitialize(C_NULL)')
        jpyobj = self.jcall('PyObject')
        self._j_py_obj = jpyobj

    def jcall(self, src):
        ans = self._j.jl_eval_string(src)
        if self._j.jl_typeof_str(ctypes.c_void_p(ans)) == "ErrorException":
            raise JuliaMagicError("ErrorException in Julia: %s" %src)
        else:
            return ans

    @line_cell_magic
    def julia(self, line, cell=None):
        '''
        Execute code in Julia, and pull some of the results back into the
        Python namespace.
        '''
        j = self._j
        tstr = self._j.jl_typeof_str
        src = str(line if cell is None else cell)
        ans = self.jcall(src)
        xx = j.jl_call1(ctypes.c_void_p(self._j_py_obj), ctypes.c_void_p(ans))
        pyans = j.jl_unbox_voidpointer(ctypes.c_void_p(j.jl_get_field(ctypes.c_void_p(xx), 'o')))
        ctypes.pythonapi.Py_IncRef(ctypes.py_object(pyans))
        return pyans

__doc__ = __doc__.format(
    JULIA_DOC = ' '*8 + JuliaMagics.julia.__doc__,
    )


def load_ipython_extension(ip):
    """Load the extension in IPython."""
    #ip.register_magics(JuliaMagics0)
    ip.register_magics(JuliaMagics)
