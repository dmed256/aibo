"""Decode the VT subset emitted by Emacs. Unknown display commands fail closed.

Raw PTY output is retained alongside every capture for independent inspection.
This decoder knows terminal commands only; it never imports Aibo expectations.
"""
import re
from wcwidth import wcwidth


class Terminal:
    def __init__(self, columns, rows):
        self.columns, self.rows = columns, rows
        self.fg, self.bg = '#d8dee9', '#15171c'
        self.bold = self.reverse = False
        self.row = self.col = 0
        self.top, self.bottom = 0, rows - 1
        self.saved = (0, 0)
        self.grid = [self.blank() for _ in range(rows)]

    def cell(self, char=' '):
        fg, bg = (self.bg, self.fg) if self.reverse else (self.fg, self.bg)
        return {'ch': char, 'fg': fg, 'bg': bg, 'bold': self.bold}

    def blank(self):
        return [self.cell() for _ in range(self.columns)]

    def down(self):
        if self.row == self.bottom:
            self.grid.pop(self.top)
            self.grid.insert(self.bottom, self.blank())
        else:
            self.row = min(self.row + 1, self.rows - 1)

    @staticmethod
    def color(index):
        basic = ['#000000','#cd0000','#00cd00','#cdcd00','#0000ee','#cd00cd','#00cdcd','#e5e5e5',
                 '#7f7f7f','#ff0000','#00ff00','#ffff00','#5c5cff','#ff00ff','#00ffff','#ffffff']
        if index < 16:
            return basic[index]
        if index >= 232:
            return '#'+f'{8+(index-232)*10:02x}'*3
        index -= 16
        ramp = [0, 95, 135, 175, 215, 255]
        return '#' + ''.join(f'{ramp[i]:02x}' for i in (index//36, index//6%6, index%6))

    def sgr(self, params):
        while params:
            value = params.pop(0)
            if value == 0:
                self.fg, self.bg = '#d8dee9', '#15171c'
                self.bold = self.reverse = False
            elif value in (1, 22):
                self.bold = value == 1
            elif value in (7, 27):
                self.reverse = value == 7
            elif value in (39, 49):
                setattr(self, 'fg' if value == 39 else 'bg', '#d8dee9' if value == 39 else '#15171c')
            elif value in (38, 48):
                mode = params.pop(0)
                if mode == 2:
                    color = '#' + ''.join(f'{params.pop(0):02x}' for _ in range(3))
                elif mode == 5:
                    color = self.color(params.pop(0))
                else:
                    raise ValueError(f'Unknown color mode: {mode}')
                setattr(self, 'fg' if value == 38 else 'bg', color)
            elif 30 <= value <= 37 or 90 <= value <= 97:
                self.fg = self.color(value-30 if value < 90 else value-90+8)
            elif 40 <= value <= 47 or 100 <= value <= 107:
                self.bg = self.color(value-40 if value < 100 else value-100+8)
            elif value not in (3, 4, 23, 24):
                raise ValueError(f'Unhandled SGR: {value}')

    def csi(self, command, args):
        if command in ('h', 'l') and args.startswith('?'):
            # Cursor visibility, alternate screen, bracketed paste, mouse/focus.
            allowed = {1, 12, 25, 1000, 1002, 1003, 1004, 1006, 1049, 2004}
            assert set(map(int, args[1:].split(';'))) <= allowed, args
            return
        params = [int(part or 0) for part in args.split(';')]
        n = params[0] or 1
        if command == 'm': self.sgr(params)
        elif command in ('H', 'f'):
            self.row, self.col = n-1, (params[1] or 1)-1 if len(params)>1 else 0
        elif command == 'A': self.row = max(self.top, self.row-n)
        elif command == 'B': self.row = min(self.bottom, self.row+n)
        elif command == 'C': self.col = min(self.columns-1, self.col+n)
        elif command == 'D': self.col = max(0, self.col-n)
        elif command == 'd': self.row = min(self.rows-1, n-1)
        elif command in ('G', '`'): self.col = min(self.columns-1, n-1)
        elif command == 'K':
            start, end = (self.col, self.columns) if params[0]==0 else (0, self.col+1) if params[0]==1 else (0, self.columns)
            self.grid[self.row][start:end] = [self.cell() for _ in range(end-start)]
        elif command == 'J':
            if params[0] == 2: self.grid = [self.blank() for _ in range(self.rows)]
            elif params[0] == 0:
                self.csi('K','0')
                for row in range(self.row+1,self.rows): self.grid[row] = self.blank()
            else: raise ValueError(f'Unhandled erase: {args}')
        elif command == 'r':
            self.top, self.bottom = n-1, (params[1] or self.rows)-1 if len(params)>1 else self.rows-1
            self.row = self.col = 0
        elif command in ('P', '@', 'X'):
            line = self.grid[self.row]
            if command == 'P': line[self.col:] = line[self.col+n:]+[self.cell() for _ in range(min(n,self.columns-self.col))]
            elif command == '@': line[self.col:] = ([self.cell() for _ in range(n)]+line[self.col:])[:self.columns-self.col]
            else: line[self.col:min(self.col+n,self.columns)] = [self.cell() for _ in range(min(n,self.columns-self.col))]
        elif command in ('L', 'M'):
            for _ in range(min(n,self.bottom-self.row+1)):
                self.grid.pop(self.bottom if command=='L' else self.row)
                self.grid.insert(self.row if command=='L' else self.bottom,self.blank())
        elif command in ('c', 'n'):
            pass  # Capability and cursor-position queries do not paint cells.
        elif command == 't' and params in ([22, 0], [22, 0, 0], [23, 0], [23, 0, 0]):
            pass  # xterm title-stack save/restore does not change the screen.
        else: raise ValueError(f'Unhandled CSI: {args}{command}')
        self.row = max(0,min(self.rows-1,self.row))
        self.col = max(0,min(self.columns,self.col))

    def feed(self, text):
        index = 0
        while index < len(text):
            char = text[index]; index += 1
            if char == '\x1b':
                match = re.match(r'\[([0-9;?<>]*)([ -/]*)([@-~])',text[index:])
                if match:
                    assert not match[2], f'Unhandled CSI intermediate: {match[0]!r}'
                    self.csi(match[3],match[1]); index += len(match[0]); continue
                if text[index] == ']':
                    end = re.search(r'\x07|\x1b\\',text[index:])
                    assert end, 'Unterminated OSC'
                    index += end.end(); continue
                command = text[index]; index += 1
                if command in '=>': continue
                if command in '()':
                    assert text[index] in 'B0', text[index]
                    index += 1; continue
                if command == '7': self.saved = self.row,self.col
                elif command == '8': self.row,self.col = self.saved
                elif command == 'D': self.down()
                elif command == 'M':
                    if self.row == self.top:
                        self.grid.pop(self.bottom);self.grid.insert(self.top,self.blank())
                    else: self.row -= 1
                else: raise ValueError(f'Unhandled escape: {command!r}')
            elif char == '\r': self.col = 0
            elif char == '\n': self.down()
            elif char == '\b': self.col = max(0,self.col-1)
            elif char == '\t': self.col = min(self.columns-1,(self.col//8+1)*8)
            elif char == '\x07': pass
            elif ord(char) < 32: raise ValueError(f'Unhandled control: {char!r}')
            else:
                width = wcwidth(char)
                assert width >= 0, char
                if width == 0:
                    self.grid[self.row][max(0,self.col-1)]['ch'] += char
                    continue
                if self.col+width > self.columns: self.col = 0;self.down()
                self.grid[self.row][self.col] = self.cell(char)
                if width == 2: self.grid[self.row][self.col+1] = self.cell('')
                self.col += width

    def frame(self):
        return {'columns':self.columns,'rows':self.rows,'cursor':{'row':self.row,'col':self.col},
                'lines':[''.join(cell['ch'] for cell in row) for row in self.grid], 'cells':self.grid}
