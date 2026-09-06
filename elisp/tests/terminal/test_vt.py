"""Guard the terminal decoder independently of the application under test."""
import unittest
from copy import deepcopy
from vt import Terminal


class Decode(unittest.TestCase):
    def test_cells(self):
        screen = Terminal(6,3)
        screen.feed('\x1b[2;2H\x1b[38;2;140;196;255m\x1b[48;2;48;74;104m界e\u0301')
        self.assertEqual(screen.frame()['lines'], ['      ',' 界é  ','      '])
        self.assertEqual(screen.grid[1][1]['fg'],'#8cc4ff')
        self.assertEqual(screen.grid[1][1]['bg'],'#304a68')
        self.assertEqual(screen.frame()['cursor'],{'row':1,'col':4})
        screen.feed('\x1b[1G\x1b[2K')
        self.assertEqual(screen.frame()['lines'][1],'      ')
        self.assertTrue(all(cell['bg']=='#304a68' for cell in screen.grid[1]))

    def test_scrolling(self):
        screen = Terminal(4,4)
        screen.feed('top\r\n111\r\n222\r\nend\x1b[2;3r\x1b[3;1H\n333')
        self.assertEqual(screen.frame()['lines'],['top ','222 ','333 ','end '])
        screen.feed('\x1b[2;1H\x1b[Lnew')
        self.assertEqual(screen.frame()['lines'],['top ','new ','222 ','end '])

    def test_titles(self):
        screen = Terminal(6,4)
        screen.feed('\x1b[2;4r\x1b[2;2H\x1b[1;31;44mtest\x1b7')
        before = deepcopy(vars(screen))
        for args in ('22;0', '22;0;0', '23;0', '23;0;0'):
            with self.subTest(args=args):
                screen.feed(f'\x1b[{args}t')
                self.assertEqual(vars(screen), before)

    def test_unknown_fails(self):
        for sequence in ('\x1b[1z', '\x1b[8;40;120t', '\x1b[22;9;0t'):
            with self.subTest(sequence=sequence), self.assertRaises(ValueError):
                Terminal(4,4).feed(sequence)


if __name__ == '__main__': unittest.main()
