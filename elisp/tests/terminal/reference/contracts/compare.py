"""Compare an approved reference with an independently captured terminal screen.

Usage: python contracts/compare.py expected.txt actual.txt
       python contracts/compare.py expected.json actual.json

Reference updates are deliberate design changes; this command never rewrites them.
"""
import difflib
import json
import sys
from pathlib import Path

expected, actual = map(Path, sys.argv[1:3])
if expected.suffix == '.json':
    def canonical(path):
        frame = json.loads(path.read_text())
        return json.dumps({key: frame[key] for key in ('columns', 'rows', 'regions', 'cursor', 'runs')}, indent=2).splitlines()
    before, after = canonical(expected), canonical(actual)
else:
    before, after = expected.read_text().splitlines(), actual.read_text().splitlines()
if before != after:
    print('\n'.join(difflib.unified_diff(before, after, fromfile=str(expected), tofile=str(actual), n=2)))
    sys.exit(1)
print('Exact match.')
