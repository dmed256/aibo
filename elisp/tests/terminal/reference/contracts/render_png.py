"""Rasterize the reference cell grid for inspection; these are not browser captures."""
import json
from pathlib import Path
from PIL import Image, ImageDraw, ImageFont
from wcwidth import wcwidth

root = Path(__file__).resolve().parents[1]
palette = json.loads((root / 'palette.json').read_text())
font = ImageFont.truetype('/System/Library/Fonts/Menlo.ttc', 14)
cell_w, cell_h = 9, 19
out = root / 'renders'
out.mkdir(exist_ok=True)
for file in sorted((root / 'snapshots' / '120x40').glob('*.json')):
    frame = json.loads(file.read_text())
    image = Image.new('RGB', (frame['columns'] * cell_w, frame['rows'] * cell_h), palette['base'][1])
    draw = ImageDraw.Draw(image)
    for row, runs in enumerate(frame['runs']):
        for run in runs:
            fg, bg = palette[run['face']]
            col = run['col']
            for char in run['text']:
                width = max(0, wcwidth(char))
                if not width:
                    continue
                x, y = col * cell_w, row * cell_h
                draw.rectangle((x, y, x + width * cell_w - 1, y + cell_h - 1), fill=bg)
                draw.text((x, y + 1), char, font=font, fill=fg)
                col += width
    image.save(out / f'{file.stem}.png')
print(f'Rendered {len(list(out.glob("*.png")))} terminal references at 120×40.')
