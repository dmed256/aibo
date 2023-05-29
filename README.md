![aibo](https://github.com/dmed256/aibo/assets/1812355/f89fb0b7-c6ad-4318-835f-a7fe16062272)

<p align="center">
  <img alt="aibo melpa badge" src="https://img.shields.io/badge/melpa-TODO-blue">
  <a href="https://pypi.org/project/aibo-server/"><img alt="aibo-server Python version badge" src="https://img.shields.io/pypi/v/aibo-server"></a>
</p>

`aibo` is an Emacs package that leverages OpenAI's chat API to bring ChatGPT into Emacs

[![asciicast](https://asciinema.org/a/612765.svg)](https://asciinema.org/a/612765)

## ⚠️  Disclaimer

- I'm not familiar with elisp so there might be quicky code conventions used
- It's mainly a personal project but will try hard to keep things backwards compatible
- Since I do a lot of coding, I'll be adding more utilities to help with coding flows (e.g. LSP-integration and git repo integrations) so expect potentially random features

## Features

- Quickly create conversations from anywhere in emacs
- Persist all conversations and messages, enabling search on past conversations in a SQLite DB (Default: `~/.aibo/database.db`)
- Supports function-calling API through simple function decorators ([package.py](https://github.com/dmed256/aibo/tree/main/python/aibo/core/package.py)). To add custom packages, define `AIBO_CUSTOM_PACKAGES_MODULE` to your custom Python module (e.g. `aibo.packages`).
- The `aibo` server automatically runs on emacs which includes hot-reloading (Default: `localhost:5000` on the `*Aibo server*` buffer)
- Inject images from clipboard using the `\im` shorthand (⚠️  requires gpt4-v access). Press `[RET]` on conversation `[Image:<id>]` links to open in a local browser.

## Installation

To use `aibo`, both the Python server and the Elisp package need to be installed. Here's how:

1. Clone the `aibo` repository:
```sh
git clone https://github.com/dmed256/aibo.git
```

2. Install Python dependencies:

2.1. Install using `pip`
```sh
pip install aibo-server
```

2.2. Install using the cloned git repo
```sh
cd aibo/python
pip install -e .[dev]
```

3. Update your Emacs configuration file (`~/.emacs` or `~/.emacs.d/init.el`):
```elisp
(add-to-list 'load-path "/path/to/aibo")
(require 'aibo)
```

4. Make sure your `OPENAI_API_KEY` environment variable is set

5. Restart or eval the Elisp code snippet.

Optional keybindings and Ivy buffer configurations are detailed below.

## Keybindings and Configurations

Here are my personal keybindings
```elisp
(global-set-key (kbd "C-M-h") 'aibo:homepage)
(global-set-key (kbd "C-M-s") 'aibo:message-search)
(global-set-key (kbd "M-/") 'aibo:create-conversation)
```

Hide `aibo` buffers from Ivy by adding the following:
```elisp
(add-to-list 'ivy-ignore-buffers "\\*Aibo")
```

## Usage

### Starting a new Conversation

Start a new conversation using `aibo:create-conversation` (`M-/.`)

### Homepage

Access the conversation history using `aibo:homepage` (`C-M-h`), displaying conversation history and allowing soft-deletion of conversations and title editing.

**Keybindings**

| Keybind       | Description                                            | Command                 |
| ------------- | ------------------------------------------------------ | ----------------------- |
| `C-c C-x C-r` | Refresh homepage                                       | `aibo:refresh-homepage` |
| `C-c p s`     | Find conversations based on searching message contents | `aibo:message-search`   |

### Conversation

**Keybindings**

| Keybind         | Description                                                                       | Command                                                       |
| --------------- | --------------------------------------------------------------------------------- | ------------------------------------------------------------- |
| `C-c C-x C-r`   | Refresh the conversation                                                          | `aibo:refresh-current-conversation`                           |
| `C-c C-t`       | Set the conversation title                                                        | `aibo:set-current-conversation-title`                         |
| `C-c C-k`       | Soft-delete the message at point                                                  | `aibo:remove-message-at-point`                                |
| `C-c C-x C-k`   | Soft-delete all messages at point and after                                       | `aibo:remove-messages-after-point`                            |
| `C-c C-c`       | Regenerate last message                                                           | `aibo:regenerate-current-conversation-last-assistant-message` |
| `C-c C-x C-t`   | Regenerate the conversation title based on the conversation content               | `aibo:generate-current-conversation-title`                    |
| `M-RET`         | If point is at the user input field, submit the message for an assistant response | `aibo:submit-user-message`                                    |


## Customization

For Elisp customizations, refer to [aibo-custom.el](https://github.com/dmed256/aibo/blob/main/elisp/aibo-custom.el).

For Python environment customizations, refer to [constants.py](https://github.com/dmed256/aibo/blob/main/python/aibo/common/constants.py).
