"""Check stage ordering and flags with mocked builds and LLVM tools.

Run with python3 contrib/optimized/test_makefile.py. No Julia build is started.
"""

import itertools
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

SOURCE = Path(__file__).with_name("Makefile")
HELPER = r'''
import json, os, pathlib, sys
root = pathlib.Path(__file__).parent
args = sys.argv[1:]
if args[0] == 'build':
    args.append({k: os.getenv(k) for k in ['PATH', 'LLVM_PROFILE_FILE']})
with (root / 'events').open('a') as f:
    f.write(json.dumps([pathlib.Path.cwd().name, args]) + '\n')
if args[0] == 'configure':
    p = pathlib.Path(args[1])
    if p.exists():
        sys.exit('configure called on existing directory')
    (p / 'deps').mkdir(parents=True)
    recipe = 'all julia-deps julia-src-release julia-symlink julia-libccalltest julia-libccalllazyfoo julia-libccalllazybar julia-libllvmcalltest:\n\t@python3 ' + str(root / 'helper.py') + ' build $@ "$(CFLAGS)" "$(LDFLAGS)" "$(JULIA_CPU_TARGET)" "$(USE_BINARYBUILDER_LLVM)" "$(LD)" "$(WIN_LD_USE_DEF)" "$(WIN_LD_EXTRA_LIBS)" "$(LINK_LDFLAGS)"\n'
    (p / 'Makefile').write_text(recipe)
    (p / 'deps/Makefile').write_text('%:\n\t@python3 ' + str(root / 'helper.py') + ' install $@ "$(USE_BINARYBUILDER_LLVM)" "$(OS)" "$(USE_BINARYBUILDER_CSL)"\n')
elif args[0] == 'install' and args[3] == 'WINNT':
    stage = pathlib.Path.cwd().parent
    (stage / 'usr/bin').mkdir(parents=True, exist_ok=True)
    (stage / 'usr/tools').mkdir(parents=True, exist_ok=True)
    (stage / 'usr/bin/support.dll').write_text('support')
    runtime = stage / 'usr/lib/clang/22/lib/windows'
    runtime.mkdir(parents=True, exist_ok=True)
    (runtime / 'libclang_rt.profile-x86_64.a').write_text('runtime')
elif args[0] == 'build':
    p = pathlib.Path.cwd()
    if p.name == 'pgo-instrumented.build' and args[1] == 'all':
        (root / 'contrib/optimized/profiles').mkdir(exist_ok=True)
        (root / 'contrib/optimized/profiles/test.profraw').write_text('profile')
    elif (p.name == 'optimized.build' or p == root) and args[1] in ['all', 'julia-src-release']:
        lib = p / 'usr/lib'
        lib.mkdir(parents=True, exist_ok=True)
        for name in ['libLLVM.so', 'libjulia-internal.so', 'libjulia-codegen.so']:
            if not (lib / name).exists():
                (lib / (name + '.1')).write_text('original')
                (lib / name).symlink_to(name + '.1')
            if args[1] == 'all' and (root / 'contrib/optimized/bolt-instrument').exists():
                (root / ('contrib/optimized/profiles-bolt/' + name + '.1-prof.123.fdata')).write_text('raw\n')
elif args[0] == 'merge':
    assert args[-1].endswith('.profraw') and pathlib.Path(args[-1]).exists(), args
    pathlib.Path(args[1].split('=', 1)[1]).write_text('merged')
elif args[0] == 'fdata':
    assert all('.merged.' not in x and pathlib.Path(x).is_file() for x in args[1:]), args
    print(''.join(pathlib.Path(x).read_text() for x in args[1:]), end='')
elif args[0] == 'bolt':
    if os.getenv('FAIL_BOLT'):
        sys.exit(1)
    assert pathlib.Path(args[1]).read_text() == 'original'
    pathlib.Path(args[args.index('-o') + 1]).write_text('instrumented' if '--instrument' in args else 'optimized')
'''

class FlowTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix='opt-unify-review-')
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.flow = self.root / 'contrib/optimized'
        self.flow.mkdir(parents=True)
        shutil.copyfile(SOURCE, self.flow / 'Makefile')
        (self.root / 'helper.py').write_text(HELPER)
        (self.root / 'Makefile').write_text('OS := Linux\nARCH := x86_64\nBINARY := 64\nBUILD_MACHINE := test-triple\nprint-%:\n\t@echo "$*=$($*)"\nconfigure:\n\t@python3 helper.py configure $(O)\n')
        for name, mode in [('profdata', 'merge'), ('mergefdata', 'fdata'), ('bolt-tool', 'bolt')]:
            p = self.root / name
            # profdata already gets 'merge' as the first argument.
            prefix = '' if name == 'profdata' else mode + ' '
            p.write_text('#!/bin/sh\nexec python3 ' + str(self.root / 'helper.py') + ' ' + prefix + '"$@"\n')
            p.chmod(0o755)
        self.args = ['LLVM_PROFDATA=' + str(self.root / 'profdata'), 'LLVM_MERGEFDATA=' + str(self.root / 'mergefdata'), 'LLVM_BOLT=' + str(self.root / 'bolt-tool')]

    def make(self, *args, ok=True, env=None):
        p = subprocess.run(['make', '--no-print-directory', '-s', '-j4', '-C', str(self.flow), *self.args, *args], text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, env=env)
        if ok:
            self.assertEqual(p.returncode, 0, p.stdout)
        else:
            self.assertNotEqual(p.returncode, 0, p.stdout)
        return p.stdout

    def events(self):
        return [json.loads(x) for x in (self.root / 'events').read_text().splitlines()]

    def test_matrix(self):
        for pgo, lto, bolt in itertools.product('01', repeat=3):
            with self.subTest(pgo=pgo, lto=lto, bolt=bolt):
                # Each configuration needs its own outputs and stamps.
                for child in self.flow.iterdir():
                    if child.name != 'Makefile':
                        shutil.rmtree(child) if child.is_dir() else child.unlink()
                (self.root / 'events').unlink(missing_ok=True)
                knobs = [f'USE_PGO={pgo}', f'USE_LTO={lto}', f'USE_BOLT={bolt}', 'JULIA_CPU_TARGET=generic;haswell']
                self.make('all', *knobs)
                events = self.events()
                builds = [(d, a) for d, a in events if a[0] == 'build']
                self.assertEqual(any(d == 'pgo-instrumented.build' for d, a in builds), pgo == '1')
                for d, a in builds:
                    if d == 'pgo-instrumented.build':
                        self.assertIn('-fprofile-generate', a[2])
                        self.assertNotIn('-fprofile-use', a[2])
                        self.assertEqual(a[4], 'generic')
                    else:
                        self.assertEqual('-fprofile-use' in a[2], pgo == '1')
                        self.assertEqual('-flto=thin' in a[2], lto == '1')
                        self.assertEqual(a[4], 'generic;haswell')
                    self.assertEqual(a[5], '0')
                self.assertTrue(all(a[2] == '1' for d, a in events if a[0] == 'install'))
                self.assertEqual(any(a[:2] == ['install', 'install-BOLT'] for d, a in events), bolt == '1')
                if bolt == '1':
                    self.assertEqual((self.flow / 'optimized.build/usr/lib/libLLVM.so').read_text(), 'optimized')
                    self.assertEqual(len([a for d,a in events if a[0] == 'bolt']), 6)
                count = len(events)
                self.make('all', *knobs)
                self.assertEqual(len(self.events()), count)

    def test_override_and_dry_run(self):
        target = '--eval=review:;@echo $(HOST_OS) $(HOST_ARCH) $(USE_BOLT) $(BUILD_MACHINE) [$(LINK_ONLY_FLAGS)]'
        for osname, arch, binary, bolt, jobs in [('Linux','i686','32','0','8'), ('Linux','aarch64','64','1',''), ('WINNT','x86_64','64','0',''), ('Darwin','aarch64','64','0','')]:
            for dry in [[], ['-n']]:
                out = self.make(*dry, target, 'review', 'OS='+osname, 'ARCH='+arch, 'BINARY='+binary, 'SDKROOT=/sdk', 'LINKER=/ld')
                self.assertIn(f'{osname} {arch} {bolt} test-triple', out)
                self.assertEqual('--thinlto-jobs=' + jobs in out, jobs != '')
        out = self.make(target, 'review', 'OS=Linux', 'ARCH=i686', 'BINARY=32', 'LTO_JOBS=', 'LINKER=/ld')
        self.assertNotIn('--thinlto-jobs', out)

    def test_windows_stages(self):
        cygpath = self.root / 'cygpath'
        cygpath.write_text('#!/bin/sh\n[ "$1" = -m ] || exit 1\nprintf "C:%s\\n" "$2"\n')
        cygpath.chmod(0o755)
        env = dict(os.environ, PATH=str(self.root) + os.pathsep + os.environ['PATH'])
        self.make('all', 'OS=WINNT', 'EXE=.exe', 'LDFLAGS=--no-insert-timestamp', env=env)
        events = self.events()
        installs = [a for d, a in events if a[0] == 'install']
        self.assertTrue({'install-csl', 'install-zlib', 'install-zstd'} <= {a[1] for a in installs})
        self.assertTrue(all(a[4] == '1' for a in installs))
        self.assertFalse(any(a[1] == 'install-BOLT' for a in installs))
        stage0 = self.flow / 'toolchain'
        self.assertEqual((stage0 / 'usr/tools/support.dll').read_text(), 'support')
        for directory, args in events:
            if args[0] != 'build':
                continue
            self.assertEqual(args[6], str(stage0 / 'usr/tools/ld.lld.exe'))
            self.assertEqual(args[7], '1')
            self.assertEqual(args[9], '--no-insert-timestamp')
            self.assertIn('-pthread', args[3])
            self.assertNotIn('--undefined-version', args[3])
            self.assertNotIn('--emit-relocs', args[3])
            self.assertEqual(args[-1]['PATH'], env['PATH'] + ':' + str(self.flow / directory / 'usr/bin'))
            if directory == 'pgo-instrumented.build':
                self.assertEqual(args[8], str(stage0 / 'usr/lib/clang/22/lib/windows/libclang_rt.profile-x86_64.a'))
                self.assertNotIn('-flto', args[2])
                if args[1] == 'julia-deps':
                    self.assertEqual(args[-1]['LLVM_PROFILE_FILE'], 'C:' + str(self.flow / directory / 'deps-profiles/%m.profraw'))
            else:
                self.assertEqual(args[8], '')
                self.assertNotIn('-gline-tables-only', args[2])
                self.assertIn('-flto=thin', args[2])
                self.assertIn('-fprofile-use=', args[2])

    def test_bolt_flags_per_architecture(self):
        # cdsplit and the jump table mode are x86-only; BOLT rejects the former
        # on AArch64 and ignores the latter.
        target = '--eval=review:;@echo "[$(BOLT_ARGS)][$(BOLT_SPLIT_STRATEGY)]"'
        for arch, x86_only in [('x86_64', True), ('aarch64', False)]:
            out = self.make(target, 'review', 'OS=Linux', 'ARCH=' + arch)
            self.assertEqual('-jump-tables=move' in out, x86_only, out)
            self.assertEqual('-split-strategy=cdsplit' in out, x86_only, out)
            self.assertIn('-reorder-blocks=ext-tsp', out)

    def test_custom_bolt_profiles(self):
        self.make('bolt-train')
        profile = self.flow / 'profiles/merged.prof'
        mtime = profile.stat().st_mtime_ns
        self.make('clean-bolt-profiles')
        self.assertTrue(profile.exists())
        self.assertTrue((self.flow / 'profiles-bolt').is_dir())
        for name in ['libLLVM.so', 'libjulia-internal.so', 'libjulia-codegen.so']:
            (self.flow / f'profiles-bolt/{name}.1-prof.456.fdata').write_text('custom\n')
        self.make('bolt')
        merged = self.flow / 'profiles-bolt/libLLVM.so.1-prof.merged.fdata'
        self.assertEqual(merged.read_text(), 'custom\n')
        new = self.flow / 'profiles-bolt/libLLVM.so.1-prof.789.fdata'
        new.write_text('extra\n')
        self.make('bolt')
        self.assertEqual(merged.read_text(), 'custom\nextra\n')
        self.assertEqual(profile.stat().st_mtime_ns, mtime)

    def test_missing_library_fails(self):
        self.make('stage2')
        (self.flow / 'optimized.build/usr/lib/libLLVM.so').unlink()
        self.make('bolt-originals', ok=False)
        self.assertFalse((self.flow / 'bolt-originals').exists())

    def test_instrument_failure_stops_chain(self):
        self.make('all', ok=False, env=dict(os.environ, FAIL_BOLT='1'))
        self.assertFalse((self.flow / 'bolt-instrument').exists())
        self.assertFalse((self.flow / 'bolt-train').exists())

    def test_restore_preserves_mtime_and_all_rewrites_again(self):
        self.make('all')
        lib = self.flow / 'optimized.build/usr/lib/libLLVM.so.1'
        mtime = lib.stat().st_mtime_ns
        self.make('restore-originals')
        self.assertEqual(lib.read_text(), 'original')
        self.assertEqual(lib.stat().st_mtime_ns, mtime)
        self.make('all')
        self.assertEqual(lib.read_text(), 'optimized')
        self.assertEqual(lib.stat().st_mtime_ns, mtime)

    def test_source_checkout_as_stage2(self):
        with (self.root / 'Makefile').open('a') as f:
            f.write('.DEFAULT_GOAL := all\nall julia-src-release julia-symlink julia-libccalltest '
                    'julia-libccalllazyfoo julia-libccalllazybar julia-libllvmcalltest:\n'
                    '\t@python3 helper.py build $@ "$(CFLAGS)" "$(LDFLAGS)" '
                    '"$(JULIA_CPU_TARGET)" "$(USE_BINARYBUILDER_LLVM)"\n')
        self.make('all', 'STAGE2_BUILD=' + str(self.root))
        self.assertEqual((self.root / 'usr/lib/libLLVM.so').read_text(), 'optimized')

    def test_clean_does_not_reconfigure_existing_directories(self):
        self.make('all', 'USE_BOLT=0')
        self.make('clean')
        self.make('all', 'USE_BOLT=0')

if __name__ == '__main__':
    unittest.main(verbosity=2)
