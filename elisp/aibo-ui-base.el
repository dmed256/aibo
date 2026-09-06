;;; aibo-ui-base.el --- Shared UI state and display helpers -*- lexical-binding: t -*-

(require 'button)
(require 'cl-lib)
(require 'face-remap)
(require 'seq)
(require 'subr-x)
(require 'time-date)
(require 'sh-script)
(require 'aibo-custom)
(require 'aibo-api)

(declare-function linum-mode "linum")

(defvar aibo:layout 'cowork)
(defvar aibo:page nil)
(defvar aibo:pre-full-window-configuration nil)
(defvar aibo:current-chat nil)
(defvar aibo:chats nil)
(defvar aibo:projects nil)
(defvar aibo:home-groups nil)
(defvar aibo:home-generation 0)
(defvar aibo:locations nil)
(defvar aibo:sidebar nil)
(defvar aibo:notification-items nil)
(defvar aibo:input-target nil)
(defvar aibo:bot-tab-offset 0)
(defvar aibo:manager-tab-offset 0)
(defvar aibo:input-attachments nil)
(defvar aibo:main-window nil)
(defvar aibo:sidebar-window nil)
(defvar aibo:input-window nil)
(defvar aibo:opening-chat-id nil)
(defvar aibo:refresh-timer nil)
(defvar aibo:refresh-inflight nil)
(defvar aibo:refresh-again nil)
(defvar aibo:refresh-generation 0)
(defvar aibo:reconcile-callback nil)
(defvar aibo:input-return-window nil)
(defvar aibo:submitting nil)
(defvar aibo:title-updates (make-hash-table :test #'equal))
(defvar-local aibo:input-chrome nil)
(defvar-local aibo:input-viewport nil)
(defvar-local aibo:edge-indicators nil)
(defvar-local aibo:input-rendering nil)
(defvar-local aibo:input-face-cookie nil)
(defvar-local aibo:rendered-attachments nil)
(defvar-local aibo:expanded-groups nil
  "Expanded hidden message identities, retained while groups grow or regroup.")
(defvar-local aibo:raw-messages nil
  "Message identities displayed as raw text rather than pretty content.")
(defvar-local aibo:input-start nil)
(defconst aibo:input-prefix-end 4 "Position after the three control rows.")
(defvar-local aibo:buffer-chat nil)
(defvar-local aibo:elapsed-timer nil)
(defvar-local aibo:header-face-cookies nil)
(defvar-local aibo:shell-cache nil)

(defconst aibo:sidebar-buffer "*Aibo notifications*")
(defconst aibo:home-buffer "*Aibo home*")
(defconst aibo:input-buffer "*Aibo input*")

(defun aibo:--get (object key)
  (and object (gethash key object)))

(defun aibo:--active-p (chat)
  (aibo:--get chat "active"))

(defun aibo:--public-chat-p (chat)
  (member (aibo:--get chat "kind") '("manager" "bot")))

(defun aibo:--chat-label (chat)
  (or (aibo:--get chat "label") "new m"))

(defun aibo:--badge-face (chat)
  (if (string= (aibo:--get chat "kind") "bot")
      'aibo:bot-badge-face
    'aibo:manager-badge-face))

(defun aibo:--message-face (chat)
  (if (string= (aibo:--get chat "kind") "bot")
      'aibo:bot-message-face
    'aibo:manager-message-face))

(defun aibo:--status-circle (chat)
  (let ((status (aibo:--get chat "status")))
    (cond ((aibo:--active-p chat)
           (propertize "●" 'face 'aibo:green-face))
          ((member status '("interrupted" "cancelled" "error" "blocked"))
           (propertize "●" 'face 'aibo:error-face))
          (t (propertize "●" 'face 'aibo:muted-face)))))

(defun aibo:--badge (chat &optional omit-location integrated-status)
  (let* ((location (aibo:--get chat "location"))
         (location-name (unless (or omit-location (equal (aibo:--get chat "kind") "manager"))
                          (aibo:--get location "name"))))
    (concat (if integrated-status
                (let ((text (concat " " (aibo:--status-circle chat) " "
                                    (aibo:--chat-label chat) " ")))
                  (add-face-text-property 0 (length text) (aibo:--badge-face chat) t text)
                  text)
              (concat (aibo:--status-circle chat) " "
                      (propertize (format " %s " (aibo:--chat-label chat))
                                  'face (aibo:--badge-face chat))))
            (and location-name
                 (propertize (format " %s " location-name)
                             'face 'aibo:location-face)))))

(defun aibo:--relative-time (timestamp)
  (condition-case nil
      (let* ((then (date-to-time (or timestamp (error "No timestamp"))))
             (seconds (float-time (time-subtract (current-time) then))))
        (cond ((< seconds 60) "now")
              ((< seconds 3600) (format "%dm" (/ (truncate seconds) 60)))
              ((< seconds 86400) (format "%dh" (/ (truncate seconds) 3600)))
              (t (format-time-string "%b %-d" then))))
    (error "")))

(defun aibo:--chat-buffer-name (chat)
  (format "*Aibo %s · %s*"
          (aibo:--chat-label chat)
          (aibo:--chat-title chat)))

(defun aibo:--chat-title (chat)
  (or (gethash (aibo:--get chat "id") aibo:title-updates)
      (aibo:--get chat "title")))

(defun aibo:--title-text (chat width)
  (propertize (truncate-string-to-width (aibo:--chat-title chat) width nil nil "…")
              'aibo-title-chat-id (aibo:--get chat "id") 'aibo-title-width width))

(defun aibo:--content-width ()
  (cond ((get-buffer-window) (window-total-width (get-buffer-window)))
        ((and (eq aibo:layout 'full) (window-live-p aibo:main-window))
         (window-total-width aibo:main-window))
        ((eq aibo:layout 'full) (- (frame-width) aibo:sidebar-width 1))
        (t (window-total-width))))

(defun aibo:--row-title (chat &optional omit-location)
  (aibo:--title-text chat
                     (max 1 (- (aibo:--content-width)
                               (string-width (aibo:--badge chat omit-location)) 4))))

(defmacro aibo:--preserve-view (&rest body)
  "Render BODY without moving the buffer's point or its windows' scroll."
  (declare (indent 0))
  `(let ((saved-point (point))
         (views (mapcar (lambda (window)
                          (list window (window-start window) (window-point window)))
                        (get-buffer-window-list (current-buffer) nil t))))
     ,@body
     (goto-char (min saved-point (point-max)))
     (dolist (view views)
       (when (window-live-p (car view))
         (set-window-start (car view) (min (cadr view) (point-max)) t)
         (set-window-point (car view) (min (caddr view) (point-max)))))))

(provide 'aibo-ui-base)
;;; aibo-ui-base.el ends here
