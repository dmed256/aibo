"""Native header rows stay pinned while the real terminal scrolls messages."""
import os
from pathlib import Path
import tempfile
import unittest

import pexpect
from capture import ROOT
from vt import Terminal


class Header(unittest.TestCase):
    def test_scroll(self):
        directory = Path(tempfile.mkdtemp(prefix='aibo-header-'))
        probe = directory / 'probe.el'
        probe.write_text('''
(with-selected-window aibo:main-window
  (let ((chat (copy-hash-table aibo:buffer-chat))
        (message (make-hash-table :test #'equal)))
    (puthash "kind" "assistant" message)
    (puthash "content" (mapconcat (lambda (n) (format "Message line %d" n))
                                 (number-sequence 1 150) "\\n") message)
    (puthash "messages" (list message) chat)
    (aibo:render-chat chat)
    (goto-char (point-min))
    (set-window-start nil (point-min))))
(global-set-key [f12]
 (lambda () (interactive)
   (with-selected-window aibo:main-window (scroll-up 8))
   (run-at-time 0.1 nil
    (lambda () (redisplay t) (send-string-to-terminal "\\e]777;scrolled\\a")))))
''')
        for columns, rows in [(96, 32), (120, 40), (400, 80)]:
            with self.subTest(size=(columns, rows)):
                child = pexpect.spawn('emacs', [
                    '-nw', '-Q', '--eval', '(setq load-prefer-newer t)',
                    '-L', str(ROOT.parents[1]), '-l', str(ROOT / 'fixture.el'),
                    '-l', str(probe),
                ], dimensions=(rows, columns), encoding='utf-8', timeout=10,
                    env={**os.environ, 'TERM': 'xterm-256color',
                         'COLORTERM': 'truecolor', 'AIBO_TERMINAL_SCENARIO': 'chat-bot'})
                raw = ''
                try:
                    while True:
                        index = child.expect_exact(['\x1b]777;aibo-ready\x07', '\x1b[>0c'])
                        raw += child.before
                        if index == 0:
                            break
                        child.send('\x1b[?1;2c')
                    screen = Terminal(columns, rows)
                    screen.feed(raw)
                    before = screen.frame()
                    child.send('\x1b[24~')
                    child.expect_exact('\x1b]777;scrolled\x07')
                    raw += child.before
                    screen.feed(child.before)
                    after = screen.frame()
                    for row in (0, 1):
                        self.assertEqual(before['lines'][row][36:], after['lines'][row][36:])
                        self.assertEqual(before['cells'][row][36:], after['cells'][row][36:])
                    self.assertIn('Make the three panels stable', after['lines'][0])
                    for label in ('project', 'goal', 'tokens', 'elapsed'):
                        self.assertIn(label, after['lines'][1])
                    self.assertNotEqual(before['lines'][3][36:], after['lines'][3][36:])
                finally:
                    (directory / f'{columns}x{rows}.vt').write_text(raw)
                    child.terminate(force=True)
        print(f'Pinned header captures: {directory}')


if __name__ == '__main__':
    unittest.main()
