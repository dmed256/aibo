# Emacs terminal tests

`reference/` contains independent terminal design fixtures and expected frames.
`accepted.json` selects the scenarios run by `verify.py`; inclusion is not proof
that a scenario passes against the current implementation.

## Run

Requires Emacs, Python with `pexpect` and `wcwidth`, and Bun for reference tooling.
From the repository root:

```sh
emacs --batch -Q --eval '(setq load-prefer-newer t)' -L elisp -L elisp/tests \
  -l aibo-test-suite -f ert-run-tests-batch-and-exit
python -m unittest discover -s elisp/tests/terminal -p 'test_*.py'
python elisp/tests/terminal/verify.py
python elisp/tests/terminal/verify.py locations projects --sizes 120x40 400x80
```

The capture harness launches isolated `emacs -nw -Q` sessions at fixed PTY
sizes with API fakes and a fixed clock; it makes no backend calls. Captures,
window geometry (`*.layout.json`), and diffs go to
`/tmp/aibo-emacs-captures/{size}/` by default. Use `verify.py --output` to change
that directory. Unknown terminal painting commands and missing fixtures fail.

## Comparison contract

The reference renderer never loads Emacs code; the capture harness never imports
that renderer. Comparisons include trailing spaces, the blank echo row, cursor
coordinates, text foregrounds/backgrounds, and blank-cell backgrounds. Hardware
cursor faces, space foregrounds, and the inactive echo area's background are
excluded. Font family belongs to the terminal.

ERT tests cover behavior such as submission, stale replies, reconnects, copy,
links, and focus preservation. Terminal header and resize tests exercise real
PTY behavior. Static frames alone do not establish those interaction contracts.

For intentional design changes, follow the [reference workflow](reference/README.md).
Never regenerate expectations from actual Emacs output merely to make tests pass.
