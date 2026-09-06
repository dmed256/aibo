;;; aibo-ui-chat.el --- Chat buffers, modes and pinned headers -*- lexical-binding: t -*-

(require 'aibo-ui-base)
(defvar aibo:notification-map)
(declare-function aibo:--expand-hidden "aibo-ui-messages")
(declare-function aibo:--insert-messages "aibo-ui-messages")
(declare-function aibo:homepage "aibo-ui-pages")
(declare-function aibo:help "aibo-ui-pages")
(declare-function aibo:render-sidebar "aibo-ui-sidebar")
(declare-function aibo:refresh-sidebar "aibo-ui-sidebar")
(declare-function aibo:render-input "aibo-ui-input")
(declare-function aibo:focus-input "aibo-ui-input")
(declare-function aibo:--page-redisplay "aibo-ui-layout")
(declare-function aibo:--sidebar-redisplay "aibo-ui-layout")
(declare-function aibo:cycle-focus "aibo-ui-layout")
(declare-function aibo:--full-layout "aibo-ui-layout")
(declare-function aibo:manager-dispatch "aibo-ui-layout")
(declare-function aibo:--cowork-dividers "aibo-ui-layout")
(declare-function aibo:cowork-delete-window "aibo-ui-layout")
(declare-function aibo:cowork-delete-other-windows "aibo-ui-layout")
(declare-function aibo:cowork-split-below "aibo-ui-layout")
(declare-function aibo:cowork-split-right "aibo-ui-layout")
(declare-function linum-mode "linum")

(defun aibo:activate ()
  "Activate the current button or row, including from its trailing space."
  (interactive)
  (let ((button (or (button-at (point)) (next-button (line-beginning-position) t))))
    (cond ((get-text-property (point) 'aibo-hidden-group)
           (let ((group (get-text-property (point) 'aibo-hidden-group)))
             (goto-char (button-start group))
             (aibo:--expand-hidden group)))
          ((and button (<= (button-start button) (line-end-position))) (push-button button))
          ((get-text-property (line-beginning-position) 'aibo-chat-id)
           (beginning-of-line) (aibo:open-chat-at-point)))))

(aibo:--define-keymap aibo:chat-mode-map
                      (let ((map (make-sparse-keymap)))
                        (set-keymap-parent map special-mode-map)
                        (define-key map (kbd "RET") #'aibo:activate)
                        (define-key map [return] #'aibo:activate)
                        (define-key map (kbd "M-/") #'aibo:focus-input)
                        (define-key map (kbd "M-m") #'aibo:toggle-message-format)
                        (define-key map (kbd "C-o") #'aibo:cycle-focus)
                        (define-key map (kbd "C-c n") aibo:notification-map)
                        (define-key map (kbd "C-c b") #'aibo:manager-dispatch)
                        (define-key map (kbd "C-c p h") #'aibo:help)
                        (define-key map (kbd "C-M-h") #'aibo:homepage)
                        (define-key map (kbd "C-x 0") #'aibo:cowork-delete-window)
                        (define-key map (kbd "C-x 1") #'aibo:cowork-delete-other-windows)
                        (define-key map (kbd "C-x 2") #'aibo:cowork-split-below)
                        (define-key map (kbd "C-x 3") #'aibo:cowork-split-right)
                        (define-key map [remap delete-window] #'aibo:cowork-delete-window)
                        (define-key map [remap delete-other-windows]
                                    #'aibo:cowork-delete-other-windows)
                        (define-key map [remap split-window-below] #'aibo:cowork-split-below)
                        (define-key map [remap split-window-right] #'aibo:cowork-split-right)
                        map))

(define-derived-mode aibo:chat-mode special-mode "Aibo"
  "Major mode for Aibo chats and pages."
  (setq-local mode-line-format nil)
  (setq-local header-line-format nil)
  (setq-local tab-line-format nil)
  (setq-local display-line-numbers nil)
  (when (fboundp 'display-line-numbers-mode) (display-line-numbers-mode -1))
  (when (bound-and-true-p linum-mode) (linum-mode -1))
  (setq-local truncate-lines nil)
  (setq-local truncate-partial-width-windows nil)
  (setq-local word-wrap t)
  (setq-local left-margin-width 1 right-margin-width 0)
  (setq-local buffer-display-table (make-display-table))
  (set-display-table-slot buffer-display-table 'vertical-border (make-glyph-code ?│ 'aibo:rule-face))
  (face-remap-add-relative 'default 'aibo:base-face)
  (face-remap-add-relative 'region 'aibo:selection-face)
  (add-hook 'pre-redisplay-functions #'aibo:--page-redisplay nil t))

(define-derived-mode aibo:sidebar-mode aibo:chat-mode "Aibo-Sidebar"
  "Notification sidebar, reachable explicitly or with the mouse."
  (setq-local truncate-lines t)
  (setq-local word-wrap nil)
  (setq-local cursor-type t)
  (setq-local line-prefix " " wrap-prefix " ")
  (setq-local left-margin-width 0 right-margin-width 0)
  (add-hook 'pre-redisplay-functions #'aibo:--sidebar-redisplay nil t))

(defun aibo:--chat-buffer (chat)
  "Find a chat by identity, even when its title has changed."
  (or (seq-find (lambda (buffer)
                  (equal (aibo:--get (buffer-local-value 'aibo:buffer-chat buffer) "id")
                         (aibo:--get chat "id")))
                (buffer-list))
      (generate-new-buffer (aibo:--chat-buffer-name chat))))

(defvar-local aibo:rendered-body nil)
(defvar-local aibo:history-loaded-p nil)

(defun aibo:--retain-history (chat)
  "Retain explicitly loaded older pages when a new latest page arrives."
  (if (and aibo:history-loaded-p (aibo:--get chat "older_before")
           (aibo:--get chat "messages"))
      (let* ((copy (copy-hash-table chat))
             (first (car (aibo:--get chat "messages")))
             (key (concat (aibo:--get first "created_at") (aibo:--get first "id")))
             (older (seq-filter
                     (lambda (message)
                       (string< (concat (aibo:--get message "created_at") (aibo:--get message "id")) key))
                     (aibo:--get aibo:buffer-chat "messages"))))
        (puthash "messages" (append older (aibo:--get chat "messages")) copy)
        (puthash "older_before" (aibo:--get aibo:buffer-chat "older_before") copy)
        copy)
    chat))

(defun aibo:--follow-bottom (window)
  "Keep the final content line visible without moving focus."
  (with-selected-window window
    (goto-char (point-max))
    (unless (bolp) (forward-line 1))
    (vertical-motion (- (window-body-height window)) window)
    (set-window-start window (point) t)
    (goto-char (max (point-min) (1- (point-max))))))

(defun aibo:--metadata-badge (label value &optional label-face value-face)
  (concat (propertize (format " %s " label) 'face (or label-face 'aibo:location-face))
          (propertize (format " %s " value) 'face (or value-face 'aibo:metadata-value-face))))

(defun aibo:--chat-metadata (chat &optional width)
  "Format pinned metadata; shorten long project names to fit WIDTH."
  (let* ((tokens (or (aibo:--get chat "tokens_used") 0))
         (seconds (or (aibo:--get chat "elapsed_seconds") 0))
         (started (aibo:--get chat "running_since"))
         (project (or (aibo:--get (aibo:--get chat "project") "name") "—"))
         (goal (if (gethash "goal_enabled" chat t)
                   (or (aibo:--get (aibo:--get chat "goal") "status") "ready")
                 "off")))
    (when started
      (setq seconds (+ seconds (max 0 (float-time (time-subtract
                                                   (current-time) (date-to-time started)))))))
    (let ((tail (concat
                 " " (aibo:--metadata-badge "goal" goal)
                 " " (aibo:--metadata-badge "tokens"
                                            (if (< tokens 1000) (number-to-string tokens) (format "%dk" (/ tokens 1000))))
                 " " (aibo:--metadata-badge "elapsed"
                                            (cond ((< seconds 60) (format "%ds" seconds))
                                                  ((< seconds 3600) (format "%dm" (/ seconds 60)))
                                                  (t (format "%dh" (/ seconds 3600))))))))
      (concat (aibo:--metadata-badge
               "project" (if width (truncate-string-to-width project (max 1 (- width (string-width tail) 11)) nil nil "…")
                           project)) tail))))

(defun aibo:--chat-header (metadata-p)
  "Return a window-sized pinned header, evaluated separately in each window."
  (when aibo:buffer-chat
    (let* ((width (max 1 (1- (window-body-width))))
           (text (if metadata-p (aibo:--chat-metadata aibo:buffer-chat width)
                   (concat (aibo:--badge aibo:buffer-chat nil t) "  "
                           (propertize (aibo:--chat-title aibo:buffer-chat) 'face 'aibo:orange-face)))))
      (propertize (truncate-string-to-width text width nil ?\s "…")
                  'help-echo (substring-no-properties text)))))

(defun aibo:--stop-elapsed-timer ()
  (when (timerp aibo:elapsed-timer) (cancel-timer aibo:elapsed-timer))
  (setq aibo:elapsed-timer nil))

(defun aibo:--tick-elapsed (buffer)
  "Redisplay pinned usage without modifying message text or scroll positions."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (force-mode-line-update))))

(defun aibo:--chat-anchor (position)
  "Remember a message-relative POSITION across changes earlier in the chat."
  (let* ((at (max (point-min) (min position (1- (point-max)))))
         (property (if (get-text-property at 'aibo-message-id)
                       'aibo-message-id 'aibo-group-id))
         (id (get-text-property at property))
         (start (or (previous-single-property-change (min (1+ at) (point-max)) property)
                    (point-min))))
    (list property id (- position start) position)))

(defun aibo:--chat-position (anchor)
  (let ((start (point-min)))
    (while (and (< start (point-max))
                (not (equal (get-text-property start (car anchor)) (nth 1 anchor))))
      (setq start (or (next-single-property-change start (car anchor)) (point-max))))
    (if (and (nth 1 anchor) (< start (point-max)))
        (min (+ start (nth 2 anchor))
             (or (next-single-property-change start (car anchor)) (point-max)))
      (min (nth 3 anchor) (point-max)))))

(defmacro aibo:--preserve-chat-view (&rest body)
  (declare (indent 0))
  `(let ((saved-point (aibo:--chat-anchor (point)))
         (saved-mark (when (mark t) (aibo:--chat-anchor (mark t))))
         (saved-mark-active mark-active)
         (views (mapcar (lambda (window)
                          (list window (aibo:--chat-anchor (window-start window))
                                (aibo:--chat-anchor (window-point window))
                                (window-vscroll window t)))
                        (get-buffer-window-list (current-buffer) nil t))))
     ,@body
     (goto-char (aibo:--chat-position saved-point))
     (when saved-mark (set-mark (aibo:--chat-position saved-mark)))
     (setq mark-active saved-mark-active)
     (dolist (view views)
       (when (window-live-p (car view))
         (set-window-start (car view) (aibo:--chat-position (nth 1 view)) t)
         (set-window-point (car view) (aibo:--chat-position (nth 2 view)))
         (set-window-vscroll (car view) (nth 3 view) t)))))

(defun aibo:toggle-message-format ()
  "Toggle raw text/JSON for the message at point, retaining other messages."
  (interactive)
  (let ((id (get-text-property (if (eobp) (max (point-min) (1- (point))) (point))
                               'aibo-message-id)))
    (unless (and aibo:buffer-chat id)
      (user-error "Place point in a message; expand hidden messages first"))
    (if (member id aibo:raw-messages)
        (setq aibo:raw-messages (delete id aibo:raw-messages))
      (push id aibo:raw-messages))
    (aibo:render-chat aibo:buffer-chat t t)))

(defun aibo:render-chat (chat &optional background preserve-view)
  "Update CHAT; BACKGROUND updates never navigate or select a window.
PRESERVE-VIEW suppresses automatic following during a formatting toggle."
  (unless (string= (aibo:--get chat "kind") "shadow")
    (let ((buffer (aibo:--chat-buffer chat)))
      (with-current-buffer buffer
        (unless (derived-mode-p 'aibo:chat-mode) (aibo:chat-mode))
        ;; A delayed GET must not replace a newer send/final snapshot.
        (when (and (aibo:--get chat "history_version")
                   (aibo:--get aibo:buffer-chat "history_version")
                   (string< (aibo:--get chat "history_version")
                            (aibo:--get aibo:buffer-chat "history_version")))
          (setq chat aibo:buffer-chat))
        (unless preserve-view (setq chat (aibo:--retain-history chat)))
        (rename-buffer (aibo:--chat-buffer-name chat) t)
        (setq-local aibo:buffer-chat chat
                    tab-line-format '(:eval (aibo:--chat-header nil))
                    header-line-format '(:eval (aibo:--chat-header t)))
        (unless aibo:header-face-cookies
          (setq aibo:header-face-cookies
                (mapcar (lambda (face)
                          (face-remap-add-relative face '(:inherit aibo:base-face :box nil :underline nil :overline nil)))
                        '(tab-line header-line))))
        (let ((body (json-encode (list (aibo:--get chat "messages") (aibo:--get chat "notice")
                                       (aibo:--get chat "older_before")
                                       (aibo:--get chat "status") (aibo:--get chat "notice_message_id")))))
          (let ((inhibit-read-only t)
                ;; Rebuild expanded bodies atomically, including shell fontification.
                (inhibit-redisplay t)
                (followers
                 (unless preserve-view
                   (seq-filter (lambda (window)
                                 (or (not (eq window (selected-window)))
                                     (>= (window-point window) (1- (point-max)))))
                               (get-buffer-window-list buffer nil t)))))
            (unless (and (not preserve-view) (equal body aibo:rendered-body))
              (aibo:--preserve-chat-view
               (erase-buffer)
               (aibo:--insert-messages chat)
               (when-let ((notice (and (not (seq-some
                                             (lambda (message)
                                               (and (equal (aibo:--get message "kind") "error")
                                                    (equal (aibo:--get message "id") (aibo:--get chat "notice_message_id"))))
                                             (aibo:--get chat "messages")))
                                       (aibo:--get chat "notice"))))
                 (insert (propertize (concat notice "\n") 'face (if (equal (aibo:--get chat "status") "error")
                                                                    'aibo:error-message-face 'aibo:muted-face)) "\n"))
               (unless (aibo:--get chat "messages")
                 (insert (propertize "No messages yet. Write below to start this conversation.\n"
                                     'face 'aibo:muted-face)))))
            (dolist (window followers)
              (aibo:--follow-bottom window))
            (unless (or preserve-view (get-buffer-window-list buffer nil t))
              (goto-char (point-max))))
          (setq aibo:rendered-body body))
        (aibo:--stop-elapsed-timer)
        (when (aibo:--get chat "running_since")
          (setq aibo:elapsed-timer (run-with-timer 1 1 #'aibo:--tick-elapsed buffer))
          (add-hook 'kill-buffer-hook #'aibo:--stop-elapsed-timer nil t)))
      (when (equal (aibo:--get aibo:input-target "id") (aibo:--get chat "id"))
        (setq aibo:input-target chat))
      (if background
          (when (equal (aibo:--get aibo:current-chat "id") (aibo:--get chat "id"))
            (setq aibo:current-chat chat))
        (setq aibo:page 'chat aibo:current-chat chat aibo:input-target chat)
        (if (eq aibo:layout 'full)
            (aibo:--full-layout buffer)
          (switch-to-buffer buffer)))
      (aibo:render-input)
      (when aibo:sidebar (aibo:render-sidebar aibo:sidebar))
      (aibo:--cowork-dividers))))

(defvar aibo:open-generation 0)

(defun aibo:open-chat-link (url)
  "Open an exact UUID or resolve a bot label at activation time."
  (let ((case-fold-search t))
    (cond
     ((string-match "\\`aibo://chat/\\([[:xdigit:]]\\{8\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{12\\}\\)\\'" url)
      (aibo:open-chat-id (downcase (match-string 1 url))))
     ((and (string-match "\\`aibo://bot/b\\(0\\|[1-9][0-9]\\{0,2\\}\\)\\'" url)
           (<= (string-to-number (match-string 1 url)) 255))
      (let ((number (string-to-number (match-string 1 url)))
            (token (cl-incf aibo:open-generation)))
        (setq aibo:opening-chat-id url)
        (aibo:api--request
         "GET" (format "/api/chats/by-bot/%d" number) nil
         (lambda (chat)
           (when (and (= token aibo:open-generation) (equal url aibo:opening-chat-id))
             (if (equal (aibo:--get chat "kind") "bot")
                 (aibo:open-chat-id (aibo:--get chat "id"))
               (message "Bot chat not found"))))
         (lambda (_error)
           (when (and (= token aibo:open-generation) (equal url aibo:opening-chat-id))
             (message "Could not resolve bot chat · draft preserved"))))))
     (t (user-error "Invalid Aibo chat link")))))

(defun aibo:open-chat-id (chat-id &optional focus-input)
  "Open CHAT-ID, focusing the composer when FOCUS-INPUT is non-nil."
  (setq aibo:opening-chat-id chat-id)
  ;; Switch immediately to a cached buffer; refresh it without stealing focus.
  (let* ((cached (seq-find
                  (lambda (buffer)
                    (let ((chat (buffer-local-value 'aibo:buffer-chat buffer)))
                      (and (member (aibo:--get chat "kind") '("bot" "manager"))
                           (equal chat-id (aibo:--get chat "id")))))
                  (buffer-list)))
         (token (cl-incf aibo:open-generation)))
    (when cached
      (aibo:render-chat (buffer-local-value 'aibo:buffer-chat cached))
      (when focus-input (aibo:focus-input)))
    (aibo:api-get-chat
     chat-id (lambda (chat)
               (when (and (= token aibo:open-generation) (equal chat-id aibo:opening-chat-id))
                 (if (member (aibo:--get chat "kind") '("bot" "manager"))
                     (progn (aibo:render-chat chat (and cached t))
                            (when (and focus-input (not cached)) (aibo:focus-input)))
                   (message "Internal chats are not visible"))))
     (lambda (_error)
       (when (and (= token aibo:open-generation) (equal chat-id aibo:opening-chat-id))
         (message "Could not load chat · select it again to retry · draft preserved"))))))

(aibo:--define-keymap aibo:chat-row-map
                      (let ((map (make-sparse-keymap)))
                        (define-key map (kbd "RET") #'aibo:open-chat-at-point)
                        (define-key map [mouse-1]
		                    (lambda (event) (interactive "e")
		                      (mouse-set-point event) (aibo:open-chat-at-point)))
                        map))

(defun aibo:open-chat-at-point ()
  (interactive)
  (if-let ((chat-id (get-text-property (point) 'aibo-chat-id)))
      (progn
        (when (derived-mode-p 'aibo:sidebar-mode)
          (aibo:api-read-chat-notifications chat-id (lambda (_response) (aibo:refresh-sidebar))))
        (aibo:open-chat-id chat-id))
    (user-error "No chat at point")))

(provide 'aibo-ui-chat)
;;; aibo-ui-chat.el ends here
