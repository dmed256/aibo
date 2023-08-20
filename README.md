![aibo](https://github.com/dmed256/aibo/assets/1812355/9ff9d25e-e47e-4fc3-a678-dd0098a03494)

> Although I've used emacs for many years, I'm completely new to elisp outside of random `.emacs` changes so the package code is probably non-optimal and lacks common code conventions

`aibo` is an Emacs package that leverages OpenAI's chat API to bring ChatGPT into Emacs. It enables the creation of conversations from anywhere using an `ivy` minibuffer and organizes persisted conversations

## Features

- Create conversations quickly using the Ivy minibuffer.
- View previous conversations in the `*Aibo homepage*`.
- Soft-delete conversations - hidden from the homepage but still accessible.
- Maintain a Python service on the `*Aibo server*` buffer with hot-reloading (Default: `localhost:5000`).
- Persist conversations in MongoDB (Default: `aibo-local` DB at `localhost:27017`).

## Upcoming Features

```
// TODO(dmed): Add demo video
```

## Installation

To use `aibo`, both the Python server and the Elisp package need to be installed. Here's how:

1. Clone the `aibo` repository:
```sh
git clone https://github.com/dmed256/aibo.git
```

2. Install and run MongoDB locally ([Mongo docs](https://www.mongodb.com/docs/manual/administration/install-community/)):
```sh
brew tap mongodb/brew
brew update
brew install mongodb-community@7.0
brew services start mongodb-community@7.0
```

3. Install Python dependencies:
```sh
cd aibo/python
pip install -e .
```

4. Update your Emacs configuration file (`~/.emacs` or `~/.emacs.d/init.el`):
```elisp
(add-to-list 'load-path "/path/to/aibo")
(require 'aibo)
```

5. Restart or eval the Elisp code snippet.

Optional keybindings and Ivy buffer configurations are detailed below.

## Keybindings and Configurations

My personal keybindings in `.emacs` include:
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

### Starting a Conversation

Start a new conversation using `aibo:create-conversation`.

### Viewing Conversation History

Access the conversation history using `aibo:homepage`. Conversations appear as follows:

```
ID: b1258d41-76a0-447f-a44b-cf39e235b067
--------------------------------------------------

[ System ]
You are an AI capable of offering comprehensive, detailed, and insightful responses to a wide array of questions. Your answers are expected to cover all aspects of the question, providing deep analysis, relevant examples, and supporting details where necessary.

[ User ]
I need a detailed and comprehensive answer to the following question, with a focus on thorough explanation and, if applicable, relevant examples: Find me good memes
```

To interact with the conversation list:
- Press `RET` on `[DEL]` to soft-delete the conversation.
- Press `RET` on `[EDIT]` to edit the conversation title.
- Press `RET` on the title to view the conversation.

## Customization

For Elisp customizations, refer to [aibo-custom.el](https://github.com/dmed256/aibo/blob/main/elisp/aibo-custom.el).

For Python environment customizations, refer to [constants.py](https://github.com/dmed256/aibo/blob/main/python/aibo/common/constants.py).
