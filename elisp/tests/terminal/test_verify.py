"""Protect the cell comparison from silently ignoring painted padding."""
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

import verify
from vt import Terminal


class Compare(unittest.TestCase):
    def test_padding(self):
        with tempfile.TemporaryDirectory(prefix='aibo-verify-') as directory:
            root = Path(directory)
            snapshots = root / 'reference' / 'snapshots' / '4x2'
            snapshots.mkdir(parents=True)
            (root / 'reference' / 'palette.json').write_text(
                json.dumps({'base': ['#ffffff', '#15171c']}))
            (snapshots / 'sample.txt').write_text('界  \n    \n')
            (snapshots / 'sample.json').write_text(json.dumps({
                'cursor': None,
                'runs': [[{'col': 0, 'text': text, 'face': 'base'}]
                         for text in ['界  ', '    ']],
            }))
            terminal = Terminal(4, 2)
            terminal.feed('\x1b[38;2;255;255;255m\x1b[48;2;21;23;28m\x1b[2J界')
            actual = terminal.frame()
            with patch.object(verify, 'ROOT', root):
                self.assertEqual(verify.compare('sample', '4x2', actual), [])
                actual['cells'][0][3]['fg'] = '#000000'
                self.assertEqual(verify.compare('sample', '4x2', actual), [])
                actual['cells'][0][3]['bg'] = '#000000'
                self.assertIn('face at 1:4', verify.compare('sample', '4x2', actual)[0])
                actual['cells'][0][3]['bg'] = '#15171c'
                actual['cells'][0][1]['bg'] = '#000000'
                self.assertIn('face at 1:2', verify.compare('sample', '4x2', actual)[0])


if __name__ == '__main__':
    unittest.main()
