"""Check loader import filtering with MinGW cross tools; no Windows execution.

Run with python3 cli/test_import_library.py. Missing toolchains are skipped.
"""

from pathlib import Path
import os
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parent.parent


class ImportLibraryTests(unittest.TestCase):
    def check_arch(self, arch, machine, use_lld=False):
        prefix = arch + '-w64-mingw32-'
        if not all(shutil.which(prefix + tool) for tool in ['gcc', 'dlltool', 'nm']):
            self.skipTest(prefix + ' tools unavailable')
        lld = os.environ.get('LLD', 'ld.lld')
        if use_lld and not shutil.which(lld):
            self.skipTest(lld + ' unavailable')
        with tempfile.TemporaryDirectory() as tmp:
            build = Path(tmp)
            (build / 'toy.c').write_text(
                '__declspec(dllexport) void jl_alloc_array_1d(void) {}\n'
                '__declspec(dllexport) void jl_codegen_only(void) {}\n'
                '__declspec(dllexport) int keep_data = 42;\n')
            if use_lld:
                subprocess.run([prefix + 'gcc', '-c', str(build / 'toy.c'),
                                '-o', str(build / 'toy.o')], check=True)
                subprocess.run([lld, '-m', 'i386pep' if arch == 'x86_64' else 'i386pe',
                                '-shared', '-e', 'jl_codegen_only' if arch == 'x86_64' else '_jl_codegen_only',
                                str(build / 'toy.o'), '-o', str(build / 'toy.dll'),
                                '--output-def', str(build / 'toy.dll.def')], check=True)
            else:
                subprocess.run([prefix + 'gcc', '-shared', str(build / 'toy.c'),
                                '-o', str(build / 'toy.dll'),
                                '-Wl,--output-def,' + str(build / 'toy.dll.def')], check=True)
            # Exercise the actual recipe with the repository's export list.
            source = (ROOT / 'cli/Makefile').read_text()
            macro = source[source.index('define implib_from_def'):source.index('DEF_FLAGS =')]
            (build / 'Makefile').write_text(
                f'CPP_STDOUT := {prefix}gcc -E\nJULIAHOME := {ROOT}\n'
                f'SRCDIR := {ROOT}/cli\nBUILDDIR := {build}\nbuild_libdir := {build}\n'
                f'DLLTOOL := {prefix}dlltool\nDLLTOOL_MACHINE := {machine}\n'
                + macro + '\nall:\n\t$(call implib_from_def,toy.dll)\n')
            subprocess.run(['make', '-s', '-C', tmp], check=True)
            symbols = subprocess.check_output([prefix + 'nm', str(build / 'toy.dll.a')], text=True)
            self.assertNotIn('jl_alloc_array_1d', symbols)
            self.assertIn('jl_codegen_only', symbols)
            self.assertIn('keep_data', symbols)
            self.assertIn('jl_alloc_array_1d', (build / 'toy.dll.def').read_text())
            (build / 'consumer.c').write_text(
                '__declspec(dllimport) void jl_codegen_only(void);\n'
                '__declspec(dllimport) int keep_data;\n'
                'int main(void) { jl_codegen_only(); return keep_data; }\n')
            subprocess.run([prefix + 'gcc', str(build / 'consumer.c'),
                            str(build / 'toy.dll.a'), '-o', str(build / 'consumer.exe')], check=True)
            # A failed preprocessor must not silently produce an unfiltered library.
            (build / 'toy.dll.a').unlink()
            result = subprocess.run(['make', '-s', '-C', tmp, 'CPP_STDOUT=false'],
                                    stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            self.assertNotEqual(result.returncode, 0)
            self.assertFalse((build / 'toy.dll.a').exists())

    def test_i686(self):
        self.check_arch('i686', 'i386')

    def test_x86_64(self):
        self.check_arch('x86_64', 'i386:x86-64')

    def test_i686_lld(self):
        self.check_arch('i686', 'i386', use_lld=True)

    def test_x86_64_lld(self):
        self.check_arch('x86_64', 'i386:x86-64', use_lld=True)


if __name__ == '__main__':
    unittest.main(verbosity=2)
