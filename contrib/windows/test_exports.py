"""Compare GNU ld and lld exports, including ThinLTO and archive selection.

Run with python3 contrib/windows/test_exports.py. CLANG and LLD can select tools.
"""

import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]


class ExportTests(unittest.TestCase):
    def check_exports(self, arch, thinlto):
        prefix = arch + '-w64-mingw32-'
        clang = os.environ.get('CLANG', 'clang')
        lld = os.environ.get('LLD', 'ld.lld')
        for tool in (prefix + 'gcc', prefix + 'ld', prefix + 'ar', clang, lld, 'llvm-ar'):
            if not shutil.which(tool):
                self.skipTest(tool + ' unavailable')
        with tempfile.TemporaryDirectory() as tmp:
            build = Path(tmp)
            export_map = build / 'julia.expmap'
            export_map.write_text((ROOT / 'src/julia.expmap.in').read_text()
                                  .replace('@JULIA_SHLIB_SYMBOL_VERSION@', 'JULIA_TEST')
                                  .replace('@LLVM_SHLIB_SYMBOL_VERSION@', 'LLVM_TEST'))
            (build / 'entry.c').write_text(
                'extern int jl_from_archive(void);\n'
                '__declspec(dllexport) int jl_entry(void) { return jl_from_archive(); }\n'
                '__declspec(dllexport) int explicit_unlisted(void) { return 2; }\n'
                'int jl_unannotated(void) { return 3; }\n'
                'int jl_data = 4;\n'
                'int unrelated(void) { return 5; }\n')
            (build / 'used.c').write_text(
                'int jl_from_archive(void) { return 6; }\n'
                'int archive_internal(void) { return 7; }\n')
            (build / 'unused.c').write_text('int jl_unused_archive_member(void) { return 8; }\n')
            definitions = []
            for compiler in ('gnu', 'llvm'):
                cc = [prefix + 'gcc', '-fvisibility=hidden'] if compiler == 'gnu' else [clang, '--target=' + arch + '-w64-mingw32']
                if compiler == 'llvm' and thinlto:
                    cc += ['-flto=thin']
                for source in ('entry', 'used', 'unused'):
                    subprocess.run([*cc, '-O2', '-c', str(build / (source + '.c')),
                                    '-o', str(build / (source + '.o'))], check=True)
                archive = build / (compiler + '.a')
                ar = prefix + 'ar' if compiler == 'gnu' else 'llvm-ar'
                subprocess.run([ar, 'rcs', str(archive), str(build / 'used.o'), str(build / 'unused.o')], check=True)
                dll = build / (compiler + '.dll')
                definition = build / (compiler + '.def')
                implib = build / (compiler + '.dll.a')
                linker = [prefix + 'ld'] if compiler == 'gnu' else [sys.executable, str(ROOT / 'contrib/windows/link_with_exports.py'), lld]
                subprocess.run([*linker, '-m', 'i386pep' if arch == 'x86_64' else 'i386pe',
                                '-shared', '-e', 'jl_entry' if arch == 'x86_64' else '_jl_entry',
                                '-o', str(dll), str(build / 'entry.o'), str(archive),
                                '--export-all-symbols', '--version-script=' + str(export_map),
                                '--out-implib', str(implib), '--output-def', str(definition)], check=True)
                self.assertTrue(implib.is_file())
                exports = {}
                for line in definition.read_text().splitlines():
                    match = re.fullmatch(r'\s*(\S+)\s+@(\d+)(\s+DATA)?\s*', line)
                    if match:
                        name, ordinal, data = match.groups()
                        exports[name] = (int(ordinal), bool(data))
                definitions.append(exports)
            self.assertEqual(definitions[0], definitions[1])
            self.assertEqual(set(definitions[1]), {'jl_entry', 'explicit_unlisted',
                                                  'jl_unannotated', 'jl_data', 'jl_from_archive'})
            self.assertTrue(definitions[1]['jl_data'][1])

    def test_x86_64(self):
        self.check_exports('x86_64', False)

    def test_x86_64_thinlto(self):
        self.check_exports('x86_64', True)

    def test_i686(self):
        self.check_exports('i686', False)

    def test_i686_thinlto(self):
        self.check_exports('i686', True)


if __name__ == '__main__':
    unittest.main(verbosity=2)
