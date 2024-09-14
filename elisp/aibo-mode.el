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
    (define-key map (kbd "C-c C-x C-r") #'aibo:refresh-current-conversation)
    (define-key map (kbd "C-c C-t")     #'aibo:set-current-conversation-title)
    (define-key map (kbd "C-c C-k")     #'aibo:remove-message-at-point)
    (define-key map (kbd "C-c C-x C-k") #'aibo:remove-messages-after-point)
    (define-key map (kbd "C-c C-e")     #'aibo:edit-message-at-point)
    (define-key map (kbd "C-c C-w")     #'aibo:copy-message-contents-at-point)
    (define-key map (kbd "C-c C-c")     #'aibo:regenerate-current-conversation-last-assistant-message)
    (define-key map (kbd "C-c C-x C-t") #'aibo:generate-current-conversation-title)
    (define-key map (kbd "M-RET")       #'aibo:submit-user-message)
    (define-key map (kbd "C-M-]")       #'aibo:submit-user-message-with-reasoning)
    map))

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
