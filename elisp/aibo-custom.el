;;; aibo-custom.el --- Customizable values -*- lexical-binding: t -*-
(require 'ht)

(defcustom aibo:server-python "python"
  "The python executable used to start the backend service"
  :type 'string
  :group 'aibo)

(defcustom aibo:server-port 5000
  "The aibo Python service will use the port defined"
  :type 'integer
  :group 'aibo)

(defcustom aibo:model "gpt-3.5-turbo-16k"
  "The OpenAI model name used for completions"
  :type 'string
  :group 'aibo)

(defcustom aibo:enable-debug-logging t
  "If set to t, adds logging in *Aibo logs*"
  :type 'boolean
  :group 'aibo)

(defcustom aibo:message-role-colors
  (ht ("system-dark"     "#377047")
      ("system-light"    "#beface")
      ("user-dark"       "#ba7a13")
      ("user-light"      "#fae2b9")
      ("assistant-dark"  "#384a8f")
      ("assistant-light" "#dce9fc")
      ("error-dark"      "#8f3838")
      ("error-light"     "#fcdcdc"))
  "Message color hash-table"
  :type 'hash-table
  :group 'aibo)

(defcustom aibo:homepage-header-foreground-color "#cccccc"
  "Header foreground color"
  :type 'color
  :group 'aibo)

(defcustom aibo:conversation-header-foreground-color "#9c9c9c"
  "Header foreground color"
  :type 'color
  :group 'aibo)

(defcustom aibo:conversation-title-foreground-color "#e9b96e"
  "Conversation title foreground color"
  :type 'color
  :group 'aibo)

(defcustom aibo:conversation-tag-foreground-color "#8cc4ff"
  "Conversation tag foreground color"
  :type 'color
  :group 'aibo)

(provide 'aibo-custom)
