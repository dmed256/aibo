"""Compare reviewed text, cursor, and cell faces with independent PTY output.

Space foregrounds and the inactive system echo area's background are not font
lock assertions. All other cells must have the exact background, including
padding; text-bearing cells must also have the exact foreground.
"""
import argparse
import difflib
import json
from pathlib import Path
from wcwidth import wcwidth
from capture import ROOT, capture


def compare(scenario, size, actual):
    root = ROOT/'reference'
    expected = json.loads((root/'snapshots'/size/f'{scenario}.json').read_text())
    lines = (root/'snapshots'/size/f'{scenario}.txt').read_text().splitlines()
    palette = json.loads((root/'palette.json').read_text())
    differences = []
    if lines != actual['lines']:
        differences.extend(difflib.unified_diff(lines, actual['lines'],fromfile='HTML reference',tofile='Emacs PTY',lineterm=''))
    if expected['cursor'] and expected['cursor'] != actual['cursor']:
        differences.append(f'cursor: expected {expected["cursor"]}, got {actual["cursor"]}')
    for row, runs in enumerate(expected['runs']):
        for run in runs:
            col = run['col']
            fg,bg = palette[run['face']]
            for char in run['text']:
                width = max(0, wcwidth(char))
                if run['face'] != 'cursor' and (char.strip() or row < len(expected['runs']) - 1):
                    for offset in range(width):
                        cell = actual['cells'][row][col + offset]
                        if bg != cell['bg'] or (char.strip() and fg != cell['fg']):
                            differences.append(f'face at {row+1}:{col+offset+1} {char!r} ({run["face"]}): expected {fg}/{bg}, got {cell["fg"]}/{cell["bg"]}')
                col += width
    return differences


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('scenarios',nargs='*')
    parser.add_argument('--sizes',nargs='+',default=['96x32','120x40','400x80'])
    parser.add_argument('--output',type=Path,default=Path('/tmp/aibo-emacs-captures'))
    args = parser.parse_args()
    report = []
    scenarios = args.scenarios or json.loads((ROOT/'accepted.json').read_text())
    for scenario in scenarios:
        for size in args.sizes:
            columns,rows = map(int,size.split('x'))
            actual = capture(scenario,columns,rows,args.output/size)
            differences = compare(scenario,size,actual)
            (args.output/size/f'{scenario}.diff').write_text('\n'.join(differences)+'\n')
            report.append({'scenario':scenario,'size':size,'passed':not differences,'differences':len(differences)})
            print(f'{scenario:24} {size:7} '+('PASS' if not differences else f'FAIL ({len(differences)} diff lines; see {args.output/size/(scenario+".diff")})'),flush=True)
    (args.output/'report.json').write_text(json.dumps(report,indent=2)+'\n')
    raise SystemExit(0 if all(row['passed'] for row in report) else 1)


if __name__ == '__main__': main()
