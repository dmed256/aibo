"""Capture an actual Emacs PTY, preserving raw VT and exact screen cells."""
import argparse
import difflib
import json
import os
from pathlib import Path

import pexpect
from vt import Terminal

ROOT = Path(__file__).resolve().parent


def capture(scenario, columns, rows, output):
    output.mkdir(parents=True, exist_ok=True)
    child = pexpect.spawn('emacs', ['-nw','-Q','--eval','(setq load-prefer-newer t)',
        '-L',str(ROOT.parents[1]),'-l',str(ROOT/'fixture.el')],
        dimensions=(rows,columns), encoding='utf-8', timeout=10,
        env={**os.environ,'TERM':'xterm-256color','COLORTERM':'truecolor','AIBO_TERMINAL_SCENARIO':scenario,
             'AIBO_TERMINAL_LAYOUT':str(output/f'{scenario}.layout.json')})
    raw = ''
    try:
        while True:
            index = child.expect_exact(['\x1b]777;aibo-ready\x07','\x1b[>0c'])
            raw += child.before
            if index == 0:
                break
            child.send('\x1b[?1;2c')
    except (pexpect.TIMEOUT, pexpect.EOF):
        raw += child.before
        raise
    finally:
        (output/f'{scenario}.vt').write_text(raw)
        child.terminate(force=True)
    terminal = Terminal(columns,rows)
    terminal.feed(raw)
    frame = terminal.frame()
    (output/f'{scenario}.txt').write_text('\n'.join(frame['lines'])+'\n')
    (output/f'{scenario}.json').write_text(json.dumps(frame,ensure_ascii=False,indent=2)+'\n')
    return frame


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('scenario')
    parser.add_argument('--size', default='120x40')
    parser.add_argument('--output', type=Path, default=Path('/tmp/aibo-emacs-captures'))
    parser.add_argument('--compare', action='store_true')
    args = parser.parse_args()
    columns,rows = map(int,args.size.split('x'))
    output = args.output/args.size
    frame = capture(args.scenario,columns,rows,output)
    print(f'Captured {args.scenario} at {args.size}: {output}')
    if args.compare:
        expected = (ROOT/'reference'/'snapshots'/args.size/f'{args.scenario}.txt').read_text().splitlines()
        diff = list(difflib.unified_diff(expected,frame['lines'],fromfile='HTML reference',tofile='Emacs PTY',lineterm=''))
        if diff:
            print('\n'.join(diff))
            raise SystemExit(1)
        print('Exact terminal text matches.')


if __name__ == '__main__':
    main()
