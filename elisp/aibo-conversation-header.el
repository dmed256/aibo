;;; aibo-conversation-header.el --- Header rendering for conversations -*- lexical-binding: t -*-
(require 'ht)
(require 'widget)

(require 'aibo-buffers)
(require 'aibo-custom)
(require 'aibo-utils)

(defconst aibo:--loader-frames
  '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"))

(defconst aibo:--loader-frame-interval 0.12)

(defvar aibo:b-loader-timer nil)
(defvar aibo:b-loader-frame-index 0)

;; ---[ Header ]----------------------------------
(defun aibo:--on-conversation-title-change (conversation)
  (let* ((conversation-id (ht-get conversation "id"))
         (conversation-buffer (aibo:buffers-get-conversation-buffer conversation-id)))
    (when conversation-buffer
      (with-current-buffer conversation-buffer
        (setq-local aibo:b-conversation conversation)
        (aibo:--set-conversation-header-line :conversation conversation)
        (aibo:--ensure-conversation-loader)
        (aibo:buffers-apply-conversation-title conversation)))
    (when (and (boundp 'aibo:homepage-buffer-name)
               (get-buffer aibo:homepage-buffer-name))
      (aibo:refresh-homepage)))
  ;; Locks down emacs for a while
  ;; (save-window-excursion (aibo:homepage))
  )

(defun aibo:--render-conversation-header (conversation)
  (let* ((id (ht-get conversation "id"))
         (cwd (ht-get conversation "cwd"))
         (header-width (max (string-width " Conversation ")
                            (string-width " CWD ")))
         (subheader-width (max (string-width (format " %s " id))
                               (if cwd (string-width (format " %s " cwd)) 0))))

    ;; Conversation
    (widget-insert
     (propertize
      (aibo:--pad-right " Conversation " header-width)
      'font-lock-face 'aibo:conversation-header-face))
    (widget-insert
     (propertize
      (aibo:--pad-right (format " %s " id) subheader-width)
      'font-lock-face 'aibo:conversation-subheader-face))
    (widget-insert "\n")

    ;; CWD (optional)
    (when cwd
      (widget-insert
       (propertize
        (aibo:--pad-right " CWD " header-width)
        'font-lock-face 'aibo:conversation-header-face))
      (widget-create
       'link
       :notify (lambda (&rest _)
                 (aibo:set-cwd))
       :button-prefix ""
       :button-suffix ""
       :tag (propertize
             (aibo:--pad-right (format " %s " cwd) subheader-width)
             'read-only t
             'font-lock-face 'aibo:conversation-subheader-face))
      (widget-insert "\n"))))

;; ---[ Loader ]----------------------------------
(defun aibo:--streaming-active-p ()
  (and (boundp 'aibo:b-streaming-websocket)
       aibo:b-streaming-websocket))

(defun aibo:--ensure-conversation-loader ()
  (if (aibo:--streaming-active-p)
      (aibo:--start-conversation-loader)
    (aibo:--stop-conversation-loader)))

;; ---[ Loader internals ]------------------------
(defun aibo:--set-conversation-header-line (&rest args)
  (let* ((conversation (plist-get args :conversation))
         (title (ht-get conversation "title")))
    (setq header-line-format
          (format "[%s][%s] %s"
                  (aibo:--conversation-loader-token)
                  aibo:model
                  title))))

(defun aibo:--conversation-loader-token ()
  (if (aibo:--streaming-active-p)
      (nth aibo:b-loader-frame-index aibo:--loader-frames)
    " "))

(defun aibo:--advance-conversation-loader-frame ()
  (setq-local aibo:b-loader-frame-index
              (mod (+ aibo:b-loader-frame-index 1)
                   (length aibo:--loader-frames))))

(defun aibo:--conversation-loader-tick (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (aibo:--streaming-active-p)
          (progn
            (aibo:--advance-conversation-loader-frame)
            (aibo:--set-conversation-header-line :conversation aibo:b-conversation)
            (force-mode-line-update))
        (aibo:--stop-conversation-loader)))))

(defun aibo:--start-conversation-loader ()
  (if aibo:b-loader-timer
      (cancel-timer aibo:b-loader-timer))
  (setq-local aibo:b-loader-frame-index 0)
  (setq-local aibo:b-loader-timer
              (run-at-time 0
                           aibo:--loader-frame-interval
                           #'aibo:--conversation-loader-tick
                           (current-buffer)))
  (aibo:--set-conversation-header-line :conversation aibo:b-conversation)
  (force-mode-line-update))

(defun aibo:--stop-conversation-loader ()
  (if aibo:b-loader-timer
      (progn
        (cancel-timer aibo:b-loader-timer)
        (setq-local aibo:b-loader-timer nil)))
  (setq-local aibo:b-loader-frame-index 0)
  (aibo:--set-conversation-header-line :conversation aibo:b-conversation)
  (force-mode-line-update))

(provide 'aibo-conversation-header)
