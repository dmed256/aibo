;;; aibo-ui.el --- Assemble the Aibo interface -*- lexical-binding: t -*-

;; Keep source reloads and byte compilation on the same module order.
(eval-and-compile
  (dolist (library '("aibo-ui-base" "aibo-ui-sidebar" "aibo-ui-messages"
                     "aibo-ui-chat" "aibo-ui-input" "aibo-ui-layout"
                     "aibo-ui-pages" "aibo-ui-events"))
    (load (concat library ".el") nil t t)))

(defun aibo:reload ()
  "Reload all Aibo source code and reconnect, preserving drafts and windows."
  (interactive)
  (aibo:api-disconnect-events)
  (cl-incf aibo:refresh-generation)
  (when (timerp aibo:refresh-timer) (cancel-timer aibo:refresh-timer))
  (setq aibo:refresh-timer nil aibo:refresh-inflight nil aibo:refresh-again nil
        aibo:reconcile-callback nil)
  (let ((load-prefer-newer t))
    (dolist (library '("aibo-custom" "aibo-api" "aibo-search" "aibo-ui" "aibo"))
      (load (concat library ".el") nil t t)))
  ;; Reconnection refreshes visible buffers.  Do not navigate home on reload:
  ;; the user may be reading a chat, using cowork windows, or editing a draft.
  (if (get-buffer aibo:input-buffer)
      (aibo:api-connect-events)
    (aibo:homepage)))

(add-hook 'aibo:api-event-functions #'aibo:--handle-event)
(add-hook 'aibo:api-connection-functions #'aibo:--connection-changed)
(add-hook 'window-configuration-change-hook #'aibo:--cowork-dividers)
(remove-hook 'window-selection-change-functions 'aibo:--guard-sidebar-selection)
(global-set-key (kbd "C-M-h") #'aibo:homepage)
(when (eq (lookup-key global-map (kbd "M-0")) #'aibo:new-manager-chat)
  (global-unset-key (kbd "M-0")))
(dolist (map (list aibo:chat-mode-map aibo:input-mode-map))
  (define-key map (kbd "M-0") #'aibo:new-manager-chat)
  (define-key map (kbd "C-c b") #'aibo:manager-dispatch)
  (define-key map (kbd "C-c p n") #'aibo:focus-sidebar)
  (define-key map (kbd "C-c p l") #'aibo:locations-page)
  (define-key map (kbd "C-c p p") #'aibo:projects-page)
  (define-key map (kbd "C-c p c") #'aibo:customization)
  (define-key map (kbd "C-c p s") #'aibo:search-chats)
  (define-key map (kbd "C-c p h") #'aibo:help)
  (define-key map (kbd "C-M-h") #'aibo:homepage))

(require 'aibo-search)

(define-key aibo:chat-mode-map (kbd "RET") #'aibo:activate)
(define-key aibo:chat-mode-map [return] #'aibo:activate)

;; Loading new source must repair existing buffers without resetting drafts.
(dolist (buffer (buffer-list))
  (with-current-buffer buffer
    (when (derived-mode-p 'aibo:chat-mode 'aibo:input-mode)
      (aibo:--enable-clipboard-key))))

(provide 'aibo-ui)
