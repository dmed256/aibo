"""Validate reference fixtures. This does not claim that Emacs matches them."""
import json
from pathlib import Path
from wcwidth import wcswidth

root = Path(__file__).resolve().parents[1]
manifest = json.loads((root / 'manifest.json').read_text())
ids = {item['id'] for item in manifest}
assert len(ids) == len(manifest), 'Duplicate scenario IDs'
checked = 0
for file in sorted((root / 'snapshots').glob('*/*.json')):
    frame = json.loads(file.read_text())
    text = file.with_suffix('.txt').read_text()
    lines = text.splitlines()
    assert len(lines) == frame['rows'], (file, 'row count', len(lines))
    assert all(wcswidth(line) == frame['columns'] for line in lines), (file, 'column count')
    assert 'HIDDEN — NEVER RENDER' not in text, (file, 'shadow leak')
    for index, runs in enumerate(frame['runs']):
        assert ''.join(run['text'] for run in runs) == lines[index], (file, index, 'cell/text mismatch')
        for run in runs:
            if run.get('action', '').startswith('scenario:'):
                assert run['action'].split(':', 1)[1] in ids, (file, 'missing action destination')
    for name, region in frame['regions'].items():
        assert region['x'] >= 0 and region['y'] >= 0, (file, name)
        assert region['x'] + region['width'] <= frame['columns'], (file, name)
        assert region['y'] + region['height'] <= frame['rows'], (file, name)
    if 'sidebar' in frame['regions']:
        assert frame['regions']['sidebar']['width'] == 35
        assert frame['regions']['main']['x'] == 36
    if 'input' in frame['regions']:
        assert 1 <= frame['regions']['input']['height'] <= 10
        assert frame['regions']['bars']['height'] == 3
        assert frame['regions']['bars']['y'] + 3 <= frame['regions']['input']['y']
    if frame['cursor']:
        assert 0 <= frame['cursor']['row'] < frame['rows']
        assert 0 <= frame['cursor']['col'] < frame['columns']
    checked += 1
for size in ('96x32', '120x40', '400x80'):
    directory = root / 'snapshots' / size
    frame = json.loads((directory / 'chat-bot.json').read_text())
    lines = (directory / 'chat-bot.txt').read_text().splitlines()
    bars = frame['regions']['bars']['y']
    assert lines[bars].index('M-1') == lines[bars + 1].index('cb0'), (size, 'shortcut alignment')
    assert 'M-0 new' in lines[bars + 1] and 'M-0' not in lines[bars]
    assert '·' not in lines[bars] + lines[bars + 1], (size, 'tab separator')
    assert not lines[-1].strip(), (size, 'unrequested footer helpers')
    assert not lines[1][:35].strip(), (size, 'section spacing')
    assert lines[2][0] == ' ' and lines[2][1] == '0', (size, 'notification padding')
    assert any(run['face'] == 'userBadge' and run['text'] == ' User '
               for row in frame['runs'] for run in row), (size, 'user badge padding')
    assert 'hidden messages] ▸' in (directory / 'hidden-collapsed.txt').read_text()
    assert 'hidden messages] ▾' in (directory / 'hidden-expanded.txt').read_text()
palette = json.loads((root / 'palette.json').read_text())
for file in (root / 'snapshots').glob('*/*.json'):
    frame = json.loads(file.read_text())
    if 'bars' in frame['regions']:
        y = frame['regions']['bars']['y']
        for row in frame['runs'][y:y+2]:
            for index, run in enumerate(row):
                if '●' in run['text']:
                    assert palette[run['face']][1] == palette[row[index-1]['face']][1], (file, 'circle background')
    if file.stem in ('locations','locations-empty','projects','projects-empty'):
        text = file.with_suffix('.txt').read_text()
        assert all(label not in text for label in ('[New location]', '[New project]', '[Edit', '[Archive]', '[Move')), (file, 'read-only reference')
print(f'{checked} reference frames verified: exact row/column counts, cell maps, bounds, links, and shadow exclusion.')
for size in ('96x32','120x40','400x80'):
    directory=root/'snapshots'/size
    empty=json.loads((directory/'home-empty.json').read_text())
    main=empty['regions']['main']
    lines=(directory/'home-empty.txt').read_text().splitlines()
    assert [line[main['x']:].strip() for line in lines[:main['height']] if line[main['x']:].strip()]==['No conversations yet']
    assert ' m   New chat' in '\n'.join(lines)
    locations=(directory/'locations.txt').read_text().splitlines()
    first=next(i for i,line in enumerate(locations) if '~/git/aibo' in line)
    assert '~/Documents/notes' in locations[first+1], (size,'location spacing')
    archived=(directory/'home-archived.txt').read_text().splitlines()
    first=next(i for i,line in enumerate(archived) if 'old-prototype' in line)
    assert not archived[first-1][36:].strip(), (size,'archive spacing')
    search=json.loads((directory/'search-many.json').read_text())
    assert all(run['face'] in ('searchInput','cursor') for run in search['runs'][0]), (size,'full gray input')
    assert search['cursor']['row']==0
    text=(directory/'search-many.txt').read_text()
    assert 'Fuzzy label' not in text and 'Search task 51' not in text
    expanded=json.loads((directory/'hidden-expanded.json').read_text())
    text=(directory/'hidden-expanded.txt').read_text().splitlines()
    for label,content in (('System','Bot instructions'),('Tool','Read elisp/aibo-ui.el'),('Event','Located the full-layout renderer')):
        row=next(i for i,line in enumerate(text) if content in line)
        assert '▏' in text[row] and label in text[row-1], (size,'expanded message chrome')
        assert any(run['face']=='internal' and content in run['text'] for run in expanded['runs'][row])
        assert not text[row+1][36:].strip(), (size,'expanded message spacing')
print('Review checks passed: empty state, new-chat badge, compact locations, archive spacing, search input, expanded messages.')
