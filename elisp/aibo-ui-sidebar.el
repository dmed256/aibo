;;; aibo-ui-sidebar.el --- Notifications and read tracking -*- lexical-binding: t -*-

(require 'aibo-ui-base)
(defvar aibo:chat-row-map)
(declare-function aibo:sidebar-mode "aibo-ui-chat")
(declare-function aibo:render-input "aibo-ui-input")
(declare-function aibo:open-chat-id "aibo-ui-chat")
(declare-function aibo:input-mode "aibo-ui-input")
(declare-function aibo:--sidebar-redisplay "aibo-ui-layout")

(defun aibo:--notification-key (index)
  (aref "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" index))

(aibo:--define-keymap aibo:notification-map
                      (let ((map (make-sparse-keymap)))
                        (dotimes (index 62)
                          (let ((item-index index))
                            (define-key map (char-to-string (aibo:--notification-key index))
                                        (lambda ()
                                          (interactive)
                                          (aibo:open-notification-index item-index)))))
                        map))

(defun aibo:--wrap-text (text width)
  "Wrap plain TEXT to WIDTH cells, preferring word boundaries."
  (mapcan
   (lambda (line)
     (let (lines)
       (while (> (string-width line) width)
         (let* ((part (truncate-string-to-width line width))
                (space (string-match " +[^ ]*$" part)))
           (when (and space (> space (/ width 2))) (setq part (substring part 0 space)))
           (push part lines)
           (setq line (string-remove-prefix " " (substring line (length part))))))
       (nreverse (cons line lines))))
   (split-string (replace-regexp-in-string "\t" "        " (or text "")) "\n")))

(defun aibo:--insert-sidebar-chat (index chat body timestamp)
  (let* ((selected (and aibo:current-chat
                        (equal (aibo:--get chat "id") (aibo:--get aibo:current-chat "id"))))
         (start (point))
         (width aibo:sidebar-width)
         (key (if (< index 62) (char-to-string (aibo:--notification-key index)) " "))
         (badge (aibo:--badge chat t))
         (time (aibo:--relative-time timestamp)))
    (insert (propertize key 'face 'aibo:muted-face) "  " badge
            (make-string (max 1 (- width 5 (string-width badge) (string-width time))) ?\s)
            (propertize time 'face (if selected 'aibo:selected-muted-face 'aibo:muted-face)) "\n")
    (insert "   " (propertize (aibo:--title-text chat (- width 5))
                              'face 'aibo:orange-face) "\n")
    (dolist (line (aibo:--wrap-text body (- width 5)))
      (insert "   " (propertize line 'face (if selected 'aibo:selected-muted-face 'aibo:muted-face)) "\n"))
    (add-text-properties start (point)
                         `(aibo-chat-id ,(aibo:--get chat "id")
                                        keymap ,aibo:chat-row-map mouse-face highlight))
    (when selected
      (save-excursion
        (goto-char start)
        (while (< (point) (point-max))
          (let ((begin (+ 2 (point))) (end (1+ (line-end-position))))
            (add-face-text-property begin end 'aibo:selected-face t)
            (put-text-property begin (1+ begin) 'display
                               (propertize "▏" 'face '(:inherit aibo:orange-face :background "#24262d"))))
          (forward-line 1))))
    (insert "\n")))

(defun aibo:--insert-active-chat (index chat)
  (let* ((start (point))
         (selected (equal (aibo:--get chat "id") (aibo:--get aibo:current-chat "id")))
         (prefix (concat (if (< index 62) (char-to-string (aibo:--notification-key index)) " ")
                         " " (if selected (propertize "▏" 'face 'aibo:orange-face) " ")
                         (aibo:--badge chat nil t) " "))
         (project (aibo:--get (aibo:--get chat "project") "name"))
         (title (concat (if project (concat project " · ") "") (aibo:--get chat "title"))))
    (insert prefix (propertize (truncate-string-to-width title
                                                         (max 1 (- aibo:sidebar-width 2 (string-width prefix))) nil nil "…")
                               'face 'aibo:muted-face) "\n")
    (add-text-properties start (point)
                         `(aibo-chat-id ,(aibo:--get chat "id")
                                        keymap ,aibo:chat-row-map mouse-face highlight
                                        help-echo ,title))
    (when selected
      (add-face-text-property (+ start 2) (point) 'aibo:selected-face t))
    (insert "\n")))

(defun aibo:--sidebar-section (title items index &optional total)
  (setq items (seq-filter (lambda (item) (aibo:--public-chat-p (or (aibo:--get item "chat") item))) items))
  (insert (propertize (format "# (%d) %s\n" (or total (length items)) title)
                      'face 'aibo:orange-face))
  (insert "\n")
  (dolist (item items)
    (let* ((notification-p (aibo:--get item "chat"))
           (chat (or (aibo:--get item "chat") item))
           (body (if notification-p (aibo:--get item "body") ""))
           (time (if notification-p
                     (aibo:--get item "created_at")
                   (aibo:--get chat "activity_at"))))
      (setq aibo:notification-items
            (append aibo:notification-items (list item)))
      (let ((start (point)))
        (if (equal title "ACTIVE") (aibo:--insert-active-chat index chat)
          (aibo:--insert-sidebar-chat index chat body time))
        (when notification-p
          (put-text-property start (point) 'aibo-notification-id (aibo:--get item "id"))))
      (setq index (1+ index))))
  ;; Notification cards and empty sections already end with a blank line.
  (unless (equal (buffer-substring-no-properties (- (point) 2) (point)) "\n\n")
    (insert "\n"))
  index)

(defun aibo:render-sidebar (sidebar)
  (setq aibo:sidebar sidebar)
  (setq aibo:notification-items nil)
  (with-current-buffer (get-buffer-create aibo:sidebar-buffer)
    (unless (derived-mode-p 'aibo:sidebar-mode) (aibo:sidebar-mode))
    (setq-local header-line-format nil)
    (let ((inhibit-read-only t)
          (index 0))
      (aibo:--preserve-view
       (erase-buffer)
       (setq index (aibo:--sidebar-section
                    "ACTIVE" (aibo:--get sidebar "active") index))
       (setq index (aibo:--sidebar-section
                    "UNREAD" (aibo:--get sidebar "unread") index))
       (aibo:--sidebar-section
	"READ" (seq-take (aibo:--get sidebar "read")
                         (max 0 (- 50 index))) index
        (or (aibo:--get sidebar "read_count") (length (aibo:--get sidebar "read"))))))
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (aibo:--sidebar-redisplay window)))
  (when (get-buffer aibo:input-buffer) (aibo:render-input)))

(defun aibo:refresh-sidebar ()
  (aibo:api-get-sidebar #'aibo:render-sidebar))

(defvar aibo:read-timer nil)
(defvar aibo:viewed-chat-id nil)
(defvar aibo:viewed-since nil)

(defun aibo:--viewed-chat-id ()
  (when (frame-focus-state)
    (with-current-buffer (window-buffer (selected-window))
      (cond (aibo:buffer-chat (aibo:--get aibo:buffer-chat "id"))
            ((and (derived-mode-p 'aibo:input-mode) (eq aibo:page 'chat)
                  aibo:current-chat
                  (get-buffer-window (aibo:--chat-buffer-name aibo:current-chat)))
             (aibo:--get aibo:current-chat "id"))))))

(defun aibo:--read-viewed-chat ()
  (let ((id (aibo:--viewed-chat-id)) (now (float-time)))
    (unless (equal id aibo:viewed-chat-id)
      (setq aibo:viewed-chat-id id aibo:viewed-since now))
    (when (and id aibo:viewed-since (>= (- now aibo:viewed-since) 15))
      (setq aibo:viewed-since now)
      (aibo:api-read-chat-notifications id (lambda (_response) (aibo:refresh-sidebar))))))

(unless (timerp aibo:read-timer)
  (setq aibo:read-timer (run-with-timer 1 1 #'aibo:--read-viewed-chat)))

(defun aibo:open-notification-index (index)
  (interactive "nNotification index: ")
  (let* ((item (nth index aibo:notification-items))
         (chat (or (aibo:--get item "chat") item))
         (chat-id (aibo:--get chat "id")))
    (unless chat (user-error "No notification at that index"))
    (aibo:api-read-chat-notifications chat-id (lambda (_response) (aibo:refresh-sidebar)))
    (aibo:open-chat-id chat-id)))

(provide 'aibo-ui-sidebar)
;;; aibo-ui-sidebar.el ends here
