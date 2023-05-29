![aibo](https://github.com/dmed256/aibo/assets/1812355/7ccbf83a-621d-4cad-90e3-e3a30e74f73b)

`aibo-server` is the backing Python server for the `aibo.el` Emacs package that leverages OpenAI's chat API to bring ChatGPT into Emacs

[![asciicast](https://asciinema.org/a/612765.svg)](https://asciinema.org/a/612765)

## Installation

This package requires Python 3.11 or greater

```sh
pip install aibo-server
```

## Start server

Normally the server will be run using the `aibo.cli.start` command in emacs

```sh
python -m aibo.cli.start
```

but it's also possible to run it using `uvicorn`

```sh
uvicorn aibo.server.main:app --port 5000
```

## Database

The server uses a simple sqlite database, by default stored in `~/.aibo/database.db`


## Customization

For Python environment customizations, refer to [constants.py](https://github.com/dmed256/aibo/blob/main/python/aibo/common/constants.py).
