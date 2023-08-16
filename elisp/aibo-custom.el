;;; aibo-custom.el --- Customizable values -*- lexical-binding: t -*-

(defcustom aibo:server-python "python"
  "The python executable used to start the backend service"
  :type 'string
  :group 'aibo)

(defcustom aibo:server-port 5000
  "The aibo Python service will use the port defined"
  :type 'integer
  :group 'aibo)

(defcustom aibo:model-name "gpt-3.5-turbo"
  "The OpenAI model name used for completions"
  :type 'string
  :group 'aibo)

(provide 'aibo-custom)
