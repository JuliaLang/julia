#!/usr/bin/env python3
"""Apply Julia's export map when linking PE images with lld's MinGW driver."""

import fnmatch
from pathlib import Path
import re
import subprocess
import sys
import tempfile


def export_patterns(path):
    # Julia's map contains named version blocks, global patterns and local: *.
    # PE has no symbol versions; only the export selection applies here.
    text = re.sub(r'/\*.*?\*/', '', Path(path).read_text(), flags=re.S)
    block = r'[^{};\s]*\s*\{([^{}]*)\}\s*;'
    if re.sub(block, '', text).strip():
        raise ValueError('unsupported export map: ' + path)
    patterns = []
    for body in re.findall(block, text):
        sections = re.split(r'\b(global|local)\s*:', body)
        if sections[0].strip():
            raise ValueError('unsupported export map: ' + path)
        for kind, entries in zip(sections[1::2], sections[2::2]):
            names = [name.strip() for name in entries.split(';') if name.strip()]
            if kind == 'global':
                patterns.extend(names)
            elif names != ['*']:
                raise ValueError('unsupported local export rule: ' + entries)
    if not patterns:
        raise ValueError('empty export map: ' + path)
    return patterns


def link(linker, args):
    maps = [arg for arg in args if arg.startswith('--version-script=')]
    if not maps:
        subprocess.run([linker, *args], check=True)
        return
    if len(maps) != 1:
        raise ValueError('expected one --version-script')
    patterns = export_patterns(maps[0].split('=', 1)[1])
    args = [arg for arg in args if arg != maps[0]]
    output_index = args.index('-o') + 1
    output = Path(args[output_index])

    # Discover exports after archive selection. Enumerating every archive member
    # up front would force otherwise unused members into the final library.
    with tempfile.TemporaryDirectory(prefix=output.name + '.', dir=output.parent) as tmp:
        definition = Path(tmp) / 'exports.def'
        probe = args.copy()
        probe[output_index] = str(Path(tmp) / output.name)
        # Do not overwrite the real import library during the discovery link.
        inputs = iter(probe)
        probe = []
        for arg in inputs:
            if arg in ('--out-implib', '--output-def'):
                next(inputs)
            elif not arg.startswith(('--out-implib=', '--output-def=')):
                probe.append(arg)
        subprocess.run([linker, *probe, '--output-def', str(definition)], check=True)
        exports = []
        for line in definition.read_text().splitlines():
            if line.strip() == 'EXPORTS':
                continue
            match = re.fullmatch(r'\s*(\S+)\s+@\d+(\s+DATA)?\s*', line)
            if not match:
                raise ValueError('unexpected lld export definition: ' + line)
            name, data = match.groups()
            if any(fnmatch.fnmatchcase(name, pattern) for pattern in patterns):
                # Let the final link assign ordinals after filtering.
                exports.append('    ' + name + (' DATA' if data else ''))
        definition.write_text('EXPORTS\n' + '\n'.join(exports) + '\n')
        # Explicit dllexport directives remain effective, just as with GNU ld's
        # version script. Disable only the unfiltered automatic exports.
        args = [arg for arg in args if arg != '--export-all-symbols']
        subprocess.run([linker, *args, '--exclude-all-symbols', str(definition)], check=True)


if __name__ == '__main__':
    try:
        link(sys.argv[1], sys.argv[2:])
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        sys.exit(str(error))
