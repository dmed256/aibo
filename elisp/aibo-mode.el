;;; aibo-mode.el --- Mode definitions -*- lexical-binding: t -*-
(require 'aibo-homepage)
(require 'aibo-conversation)

;; ---[ Keymaps ]---------------------------------
(defvar aibo:mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-x C-r") #'aibo:refresh-homepage)
    (define-key map (kbd "C-c p s")     #'aibo:message-search)
    map))

(defvar aibo:conversation-mode-map
  (let ((map (make-sparse-keymap)))
    (aibo:--set-conversation-keybindings map)))

;; ---[ Modes ]-----------------------------------
(define-derived-mode aibo:mode text-mode "Aibo"
  (kill-all-local-variables)
  (use-local-map aibo:mode-map)
  (run-hooks #'aibo:mode-hook))

(define-derived-mode aibo:conversation-mode text-mode "Aibo-Conversation"
  (kill-all-local-variables)
  (use-local-map aibo:conversation-mode-map)
  (advice-add #'end-of-buffer
              :after #'aibo:--new-user-input-end-of-buffer-advice)
  (run-hooks #'aibo:conversation-mode-hook))

(provide 'aibo-mode)
