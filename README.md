![aibo](https://github.com/dmed256/aibo/assets/1812355/f89fb0b7-c6ad-4318-835f-a7fe16062272)

# aibo

`aibo` wraps `codex` to be used in emacs or web but uses a different organization/communication system

> [!IMPORTANT]
> Aside from the original design doc and feedback on features/bugs, the code here is purely `codex` maintained so here be dragons.

Some namings:

| Name | Meaning |
| --- | --- |
| `projects` | A short description and a `.md` file for shared context |
| `location` | A named local path where a bot works, remote execution over SSH isn't implemented |
| `m#` (manager) | Default chats that choose an optional project and a location, then delegate to `b#` when needed |
| `b#` (bot) | The main conversation, with the project's `.md` context and the location as its working directory |
| notifications | Instead of a sidebar of all chats, finished chats show up in `UNREAD` and `ACTIVE` chats stay at the top |
| keybinds! | `M-1` through `M-9` open recent bots, `C-c b #` opens recent managers, `M-0` starts a new manager draft |

There are almost no buttons to edit things, it's mainly chat driven such as adding/updating new projects or locations

## Installation

Dependencies:

- `codex` in `PATH` (+ authenticated)
- Python 3.11+
- Emacs 29+ (+ `websocket` package)
- PostgreSQL
- Bun

Inside the `aibo` dir, to install the python backend:

```sh
python -m pip install -e ./python
```

Add to your `.emacs` or emacs config:

```elisp
(add-to-list 'load-path "/path/to/aibo/elisp")
(setq aibo:python-command "/path/to/python")
(require 'aibo)
```

Start up with `C-M-h` to open up aibo

## Emacs

| Keybinding | Action |
| --- | --- |
| `C-M-h` | Open the workspace or return home |
| `M-/` | Focus input, repeat to toggle the active chat and a new manager draft |
| `M-0` | Open a fresh manager draft, create the chat on send |
| `M-RET` | Send the draft |
| `RET` | Insert a newline in input, activate a link or selection elsewhere |
| `C-o` | Switch between chat and input |
| `C-c C-i` | Attach a clipboard image |
| `M-m` | Toggle raw/pretty rendering for the message at point |
| `C-c p s` | Search conversations |
| `C-c p h` | Show contextual help |
| `C-c p c` | Open customization |

Press `RET` on a file link to open text files as ordinary Emacs buffers. HTML, PDFs, images, videos, and binaries open with macOS `open`

## Web

The default is [localhost:5000](http://localhost:5000), sharing chats with emacs but with a browser-native layout, appearance settings, and attached convo images. Use `Ctrl+K` / `Cmd+K` to traverse through the app

There's an emacs tutorial as well if you want to visit it through `Ctrl+K` / `Cmd+K`

## Managers, bots, and projects

An orange **m#** is your manager. It answers small requests and delegates larger ones to purple **b#** bots, or resumes an existing bot. Managers have no assigned location, they choose named working directories for bots. Their own Codex process starts in your home directory. A bot's gray badge shows its location

Their instructions live in `~/.cache/aibo/docs/` (or `$AIBO_CACHE_DIR/docs/`).
Missing `m.md`, `b.md`, and `sb.md` files are seeded from bundled defaults;
existing files are never overwritten, so you can customize them freely.
`aibo/api.md` is managed by Aibo and refreshed from the bundled reference at
server startup and whenever role instructions are loaded; local edits are overwritten.

| File | Purpose |
| --- | --- |
| `m.md` | Coordination and delegation |
| `b.md` | How a worker carries out its task |
| `sb.md` | How outcomes are summarized |
| `aibo/api.md` | API guide used by the assistants |

Note that their numbers are convenient labels and eventually repeat (`[0,255]` inclusive range) to help with referencing to chats by shortnames (e.g. "can you check why b3 crashed?")

Hidden shadow bots (`sb`) summarize finished work for notifications, and separate short-lived Codex threads generate titles, neither clutters your conversation search

Each project expects a `README.md` in `~/.cache/aibo/projects/{project_name}/README.md` for `m#` and `b#` to have context, just ask `m` to update/create one for you

## Customization

| Client | Open customization |
| --- | --- |
| emacs | `C-c p c` |
| web | `Ctrl+K` / `Cmd+K`, then choose Customization |

## Deployment and configuration

### Start missing services

`C-M-h` does this automatically, or run it yourself:

```sh
python -m aibo.cli.deploy ensure
```

### Update the server and UI

Rebuild the server, web assets, and Emacs code, then reload an existing Emacs daemon without restarting the bouncer:

```sh
python -m aibo.cli.deploy redeploy
```

### Update the bouncer

Restart just the Codex bouncer, **this can interrupt active work**:

```sh
python -m aibo.cli.deploy bouncer-redeploy
```

### Update both

Rebuild the server and UI and restart the bouncer, with the same interruption risk:

```sh
python -m aibo.cli.deploy redeploy --redeploy-bouncer
```

### Configuration

Set `aibo:server-url` for another local port, or `aibo:auto-start` to `nil` for a server you manage yourself

Local startup keeps PostgreSQL, connection settings, and logs under `~/.cache/aibo`. `AIBO_CACHE_DIR` changes that root, `AIBO_DATABASE_URL` selects an existing database

For nginx blue/green deployment, see [deploy/nginx](deploy/nginx) and set `AIBO_PUBLIC_URL`. Use one process manager per service, don't mix launchd-owned services with the CLI's managed processes

## Continuous integration

[GitHub Actions](.github/workflows/python.yml) runs on pull requests, pushes to `main`, and manual requests, without deploying or starting real Codex turns

| Check | Coverage |
| --- | --- |
| Backend | Black, isort, Ruff, mypy, migrations, and Python tests with disposable PostgreSQL |
| Web | Prettier, ESLint, tests, TypeScript, and a production build |
| Emacs | Native formatting and syntax, strict byte compilation, ERT, and terminal behavior tests |

Branch protection needs to require these checks separately. Browser screenshot tests and the full [terminal reference matrix](elisp/tests/terminal/README.md) aren't CI gates

## Repository layout

| Directory | Contents |
| --- | --- |
| `elisp/` | Emacs entry point, API, search, settings, and UI modules grouped by responsibility |
| `elisp/tests/` | ERT tests and independent terminal reference fixtures |
| `python/src/aibo/core/` | Workspace rules, model settings, and title generation |
| `python/src/aibo/server/routes/` | API routers grouped by resource, with shared HTTP helpers |
| `python/src/aibo/bouncer/` | Codex transport and session management |
| `python/src/aibo/cli/` | Local startup, deployment, and migrations |
| `python/src/aibo/db/` | Database access and models |
| `web/src/pages/` | Main-panel views |
| `web/src/components/` | Shared UI components, composer, sidebar, and dialogs |
| `web/src/hooks/` | Browser event, keyboard, and attachment integrations |
| `web/src/state/` | Observable workspace and search state shared by views |
| `web/src/tutorial/` | Guided tutorial and generated terminal frames |
| `deploy/` | Deployment configuration templates, not local process state |
