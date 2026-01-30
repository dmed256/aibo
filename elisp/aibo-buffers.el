;;; aibo-buffers.el --- Buffer management helpers -*- lexical-binding: t -*-
(require 'ht)

;; ---[ Conversation buffers ]--------------------
(defvar aibo:buffers--conversation-by-id (make-hash-table :test 'equal))

(defun aibo:buffers-get-conversation-buffer (conversation-id)
  (let* ((buffer (gethash conversation-id aibo:buffers--conversation-by-id)))
    (if (and buffer (buffer-live-p buffer))
        buffer
      (progn
        (if buffer
            (remhash conversation-id aibo:buffers--conversation-by-id))
        nil))))

(defun aibo:buffers-register-conversation-buffer (conversation-id buffer)
  (if (and conversation-id buffer)
      (progn
        (puthash conversation-id buffer aibo:buffers--conversation-by-id)
        (with-current-buffer buffer
          (setq-local aibo:b-conversation-id conversation-id)
          (add-hook 'kill-buffer-hook #'aibo:buffers--conversation-kill-buffer-hook nil t)))))

(defun aibo:buffers-unregister-conversation-buffer (conversation-id buffer)
  (let* ((current-buffer (gethash conversation-id aibo:buffers--conversation-by-id)))
    (if (and current-buffer (eq current-buffer buffer))
        (remhash conversation-id aibo:buffers--conversation-by-id))))

(defun aibo:buffers-get-or-create-conversation-buffer (&rest args)
  (let* ((conversation-id (plist-get args :conversation-id))
         (open-style (or (plist-get args :open-style) :current-window))
         (on-create (plist-get args :on-create))
         (on-load (plist-get args :on-load))
         (buffer (aibo:buffers-get-conversation-buffer conversation-id))
         (did-create nil))
    (if (null buffer)
        (progn
          (setq buffer (aibo:buffers--find-conversation-buffer conversation-id))
          (if buffer
              (aibo:buffers-register-conversation-buffer conversation-id buffer))))
    (if (null buffer)
        (progn
          (setq buffer (generate-new-buffer
                        (aibo:buffers--conversation-default-buffer-name conversation-id)))
          (setq did-create t)
          (aibo:buffers-register-conversation-buffer conversation-id buffer)))
    (aibo:buffers--open-buffer buffer open-style)
    (if (and did-create on-create)
        (with-current-buffer buffer
          (funcall on-create buffer)))
    (if on-load
        (with-current-buffer buffer
          (funcall on-load buffer)))
    buffer))

(defun aibo:buffers-apply-conversation-title (conversation)
  (let* ((conversation-id (ht-get conversation "id"))
         (title (ht-get conversation "title"))
         (sanitized-title (aibo:buffers--sanitize-title title))
         (buffer (current-buffer)))
    (aibo:buffers-register-conversation-buffer conversation-id buffer)
    (if (and sanitized-title (> (length sanitized-title) 0))
        (progn
          (setq-local list-buffers-directory sanitized-title)
          (aibo:buffers--rename-conversation-buffer conversation-id sanitized-title))
      (progn
        (setq-local list-buffers-directory nil)
        (aibo:buffers--rename-conversation-buffer conversation-id nil)))))

;; ---[ Internal ]--------------------------------
(defun aibo:buffers--sanitize-title (title)
  (if (null title)
      nil
    (let* ((clean-title (replace-regexp-in-string "[\n\r\t]+" " " title))
           (collapsed-title (replace-regexp-in-string "[[:space:]]\\{2,\\}" " " clean-title))
           (trim-left (replace-regexp-in-string "^[[:space:]]+" "" collapsed-title)))
      (replace-regexp-in-string "[[:space:]]+$" "" trim-left))))

(defun aibo:buffers--conversation-default-buffer-name (conversation-id)
  (format "*Aibo [%s]*" conversation-id))

(defun aibo:buffers--conversation-titled-buffer-name (conversation-id title)
  (format "Aibo \"%s\" [%s]" title conversation-id))

(defun aibo:buffers--rename-conversation-buffer (conversation-id title)
  (let* ((target-name (if (and title (> (length title) 0))
                          (aibo:buffers--conversation-titled-buffer-name conversation-id title)
                        (aibo:buffers--conversation-default-buffer-name conversation-id))))
    (if (not (string= (buffer-name) target-name))
        (rename-buffer target-name t))))

(defun aibo:buffers--find-conversation-buffer (conversation-id)
  (let* ((buffers (buffer-list))
         (match nil))
    (while (and buffers (null match))
      (let* ((buffer (car buffers)))
        (if (with-current-buffer buffer
              (and (local-variable-p 'aibo:b-conversation-id)
                   (string= aibo:b-conversation-id conversation-id)))
            (setq match buffer)))
      (setq buffers (cdr buffers)))
    (if (and match (buffer-live-p match))
        match
      nil)))

(defun aibo:buffers--open-buffer (buffer open-style)
  (cond
   ((eq open-style :current-window)
    (switch-to-buffer buffer))
   ((eq open-style :new-window)
    (if (> (length (window-list)) 1)
        (switch-to-buffer buffer)
      (pop-to-buffer buffer)))))

(defun aibo:buffers--conversation-kill-buffer-hook ()
  (let* ((conversation-id (and (local-variable-p 'aibo:b-conversation-id)
                               aibo:b-conversation-id)))
    (if conversation-id
        (aibo:buffers-unregister-conversation-buffer conversation-id (current-buffer)))))

(provide 'aibo-buffers)
