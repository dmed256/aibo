"""Resize a live terminal without rebuilding its workspace or losing the draft."""
import json
import os
from pathlib import Path
import tempfile
import unittest

import pexpect
from capture import ROOT


class Resize(unittest.TestCase):
    def test_workspace(self):
        directory = Path(tempfile.mkdtemp(prefix='aibo-resize-'))
        probe = directory / 'probe.el'
        output = directory / 'state.json'
        probe.write_text('''
(defvar aibo-resize:windows (window-list))
(defvar aibo-resize:focus (selected-window))
(global-set-key [f12]
 (lambda () (interactive)
  (run-at-time 0.1 nil
   (lambda ()
    (redisplay t)
    (with-temp-file (getenv "AIBO_RESIZE_OUTPUT")
     (insert (json-serialize
      (with-current-buffer aibo:input-buffer
       `((width . ,(frame-width))
         (height . ,(frame-height))
         (sidebar . ,(window-total-width aibo:sidebar-window))
         (input . ,(window-total-width aibo:input-window))
         (windows . ,(if (equal aibo-resize:windows (window-list)) t :false))
         (focus . ,(if (eq aibo-resize:focus (selected-window)) t :false))
         (point . ,(- (point) aibo:input-start))
         (draft . ,(buffer-substring-no-properties aibo:input-start (point-max))))))))
    (send-string-to-terminal "\\e]777;aibo-resized\\a")))))
''')
        child = pexpect.spawn('emacs', [
            '-nw', '-Q', '--eval', '(setq load-prefer-newer t)',
            '-L', str(ROOT.parents[1]), '-l', str(ROOT / 'fixture.el'),
            '-l', str(probe),
        ], dimensions=(40, 120), encoding='utf-8', timeout=10, env={
            **os.environ, 'TERM': 'xterm-256color', 'COLORTERM': 'truecolor',
            'AIBO_TERMINAL_SCENARIO': 'input-cursor', 'AIBO_RESIZE_OUTPUT': str(output),
        })
        with (directory / 'session.vt').open('w') as log:
            child.logfile_read = log
            try:
                while child.expect_exact(['\x1b]777;aibo-ready\x07', '\x1b[>0c']):
                    child.send('\x1b[?1;2c')
                baseline = None
                for columns, rows in [(120, 40), (400, 80), (96, 32), (120, 40)]:
                    child.setwinsize(rows, columns)
                    child.send('\x1b[24~')
                    child.expect_exact('\x1b]777;aibo-resized\x07')
                    state = json.loads(output.read_text())
                    self.assertEqual((state['width'], state['height']), (columns, rows))
                    self.assertEqual(state['sidebar'], 36)  # 35 content cells plus divider.
                    self.assertEqual(state['input'], columns)
                    self.assertTrue(state['windows'])
                    self.assertTrue(state['focus'])
                    draft = (state['draft'], state['point'])
                    if baseline is None:
                        baseline = draft
                    self.assertEqual(draft, baseline)
            finally:
                child.terminate(force=True)


if __name__ == '__main__':
    unittest.main()
