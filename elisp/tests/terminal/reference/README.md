# Terminal design references

Independent reference frames describe the intended UI, not captured Emacs output.
Passing reference checks does not establish that the Emacs implementation matches.

## Preview the gallery

From the repository root:

```sh
cd elisp/tests/terminal/reference
bun build.mjs --gallery-only
python -m http.server 58342 --bind 127.0.0.1
```

Open http://127.0.0.1:58342/. Generated `index.html` and `views/*.html` are ignored
by Git. `--gallery-only` leaves versioned fixtures and snapshots untouched.
Navigation, draft editing, and sending are local simulations; they never call
Aibo or start Codex work. Reset fixture discards interactive changes.

## Assets

- `scenarios.js`: deterministic content, focus, draft, scroll, and status fixtures.
- `renderer.js`: independent terminal renderer, also used by the web tutorial.
- `gallery.js` and `gallery.css`: interactive review UI.
- `fixtures.json`: exported scenarios consumed by the Emacs fixture adapter.
- `manifest.json`: scenario inventory used by reference verification.
- `palette.json`: foreground/background colors used by terminal and ERT tests.
- `snapshots/{size}/{scenario}.txt`: exact text, including trailing spaces.
- `snapshots/{size}/{scenario}.json`: color runs, cursor, regions, and actions.

Frames cover 96×32, 120×40, and 400×80, including the blank Emacs echo row.
Keep snapshots and exported fixture data versioned so reference changes remain
reviewable and tests run without regenerating their expectations.

## Change references

Edit the renderer or scenarios only for deliberate design changes. From this
directory:

```sh
bun build.mjs
python contracts/verify.py
bun contracts/gallery_smoke.mjs
```

The full build updates gallery pages, snapshots, fixtures, manifest, and palette.
Review the generated diff, then run the independent [Emacs tests](../README.md).
The gallery smoke test uses a fake DOM, not a real browser.

`contracts/compare.py expected.txt actual.txt` compares independent captures
without rewriting expectations; it also accepts matching JSON frame files.
Optional `python contracts/render_png.py` rasterizes snapshots into ignored
`renders/` files; it requires Pillow, `wcwidth`, and macOS's Menlo font. These
images are not browser captures.
