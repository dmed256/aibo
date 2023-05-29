# aibo

> The package is still very much in development and I'm new to elisp, so the code is probably non-optimal and differs from normal conventions

`aibo` is an emacs package that allows you to interact with ChatGPT, which includes a python server and an elisp package

## Installation

To use `aibo`, you'll need both the python server and the elisp package installed. Follow these steps:

1. Clone the `aibo` repository

```sh
git clone https://github.com/dmed256/aibo.git
```

2. Install the python dependencies

```sh
cd aibo/python
pip install -e .
```

3. Add the following lines to your emacs configuration file (usually `~/.emacs` or `~/.emacs.d/init.el`):

```elisp
(add-to-list 'load-path "/path/to/aibo")
(require 'aibo)

;; (Optional) Set up keybindings to your taste
;; (global-set-key (kbd "C-M-h") 'aibo:homepage)
;; (global-set-key (kbd "M-/") 'aibo:create-conversation)
```

4. Restart of eval the elisp code snippet above

## Usage

Run `aibo:homepage` (or the keybinding) to view all historical conversations. It'll look something like

```
Today
  [DEL] [EDIT] Find me better memes
Yesterday
  [DEL] [EDIT] Find me good memes
Last 7 days
  [DEL] [EDIT] TEST
```

- Pressing `RET` on `[DEL]`: Soft-delete the conversation
- Pressing `RET` on `[EDIT]`: Edit the conversation title
- Pressing `RET` on the title: View the conversation

Viewing a conversation will create a new buffer with a name like `*aibo [\<short-hash>] - \<title>*` which will have the conversation. For example:

```
ID: b1258d41-76a0-447f-a44b-cf39e235b067
--------------------------------------------------

[System]
You are an AI capable of offering comprehensive, detailed, and insightful responses to a wide array of questions. Your answers are expected to cover all aspects of the question, providing deep analysis, relevant examples, and supporting details where necessary.

[User]
I need a detailed and comprehensive answer to the following question, with a focus on thorough explanation and, if applicable, relevant examples: Find me good memes
```
