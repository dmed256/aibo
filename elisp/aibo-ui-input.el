;;; aibo-ui-input.el --- Composer, attachments and submission -*- lexical-binding: t -*-

(require 'aibo-ui-base)
(defvar aibo:chat-mode-map)
(defvar aibo:notification-map)
(declare-function aibo:open-link "aibo-ui-messages")
(declare-function aibo:render-chat "aibo-ui-chat")
(declare-function aibo:--metadata-badge "aibo-ui-chat")
(declare-function aibo:--edge-indicator "aibo-ui-layout")
(declare-function aibo:cycle-focus "aibo-ui-layout")
(declare-function aibo:--recent-chats "aibo-ui-layout")
(declare-function aibo:open-recent-bot "aibo-ui-layout")
(declare-function aibo:manager-dispatch "aibo-ui-layout")
(declare-function aibo:cowork-delete-window "aibo-ui-layout")
(declare-function aibo:cowork-delete-other-windows "aibo-ui-layout")
(declare-function aibo:cowork-split-below "aibo-ui-layout")
(declare-function aibo:cowork-split-right "aibo-ui-layout")
(declare-function linum-mode "linum")

(defun aibo:--human-bytes (bytes)
  (cond ((< bytes 1024) (format "%d B" bytes))
        ((< bytes (* 1024 1024)) (format "%.1f KB" (/ bytes 1024.0)))
        (t (format "%.1f MB" (/ bytes 1048576.0)))))

(defun aibo:--remove-attachment (button)
  (let ((path (button-get button 'aibo-path)))
    (setq aibo:input-attachments (delete path aibo:input-attachments))
    (aibo:render-input)
    (aibo:--echo (concat "Removed " (file-name-nondirectory path)))))

(defun aibo:--insert-attachments ()
  (when aibo:input-attachments
    (insert (propertize "Attachments\n" 'face 'aibo:muted-face))
    (dolist (path aibo:input-attachments)
      (insert-text-button "[del]"
                          'face 'aibo:link-face 'follow-link t
                          'aibo-path path
                          'action #'aibo:--remove-attachment)
      (insert " ")
      (insert-text-button (file-name-nondirectory path)
                          'face 'aibo:link-face 'follow-link t
                          'aibo-path path
                          'action #'aibo:open-link)
      (insert "  " (propertize (aibo:--human-bytes
                                (file-attribute-size (file-attributes path))) 'face 'aibo:muted-face) "\n"))))

(defun aibo:attach-clipboard-image ()
  (interactive)
  (unless (eq system-type 'darwin)
    (user-error "Clipboard image import currently requires macOS"))
  (let* ((directory "/tmp/aibo-tmp/emacs/assets")
         (path (expand-file-name
                (format "clipboard-%s.png" (format-time-string "%s%N"))
                directory))
         (script (format
                  (concat "set imageData to the clipboard as «class PNGf»\n"
                          "set imageFile to open for access POSIX file %S with write permission\n"
                          "set eof imageFile to 0\n"
                          "write imageData to imageFile\n"
                          "close access imageFile")
                  path)))
    (make-directory directory t)
    (if (and (= 0 (call-process "osascript" nil nil nil "-e" script))
             (file-exists-p path)
             (> (file-attribute-size (file-attributes path)) 0))
        (progn
          (push path aibo:input-attachments)
          (aibo:render-input))
      (when (file-exists-p path) (delete-file path))
      (aibo:--echo "Clipboard does not contain a supported image" t))))

;; Personal minor modes (including Projectile) precede major-mode bindings.
;; Reserve Aibo's explicit bindings within Aibo buffers only.
(aibo:--define-keymap aibo:clipboard-mode-map
                      (let ((map (make-sparse-keymap)))
                        (define-key map (kbd "C-c C-i") #'aibo:attach-clipboard-image)
                        map))

(define-minor-mode aibo:clipboard-mode
  "Keep the clipboard attachment shortcut available in Aibo."
  :lighter nil :keymap aibo:clipboard-mode-map)

(defvar aibo:clipboard-emulation-map-alist
  `((aibo:clipboard-mode . ,aibo:clipboard-mode-map)))
(add-to-list 'emulation-mode-map-alists 'aibo:clipboard-emulation-map-alist)

(defun aibo:--enable-clipboard-key ()
  (aibo:clipboard-mode 1)
  (let ((map (current-local-map)) maps)
    (while (and map (not (eq map special-mode-map)))
      (let ((copy (copy-keymap map)))
        (set-keymap-parent copy nil)
        (push copy maps))
      (setq map (keymap-parent map)))
    (setq-local aibo:clipboard-emulation-map-alist
                `((aibo:clipboard-mode . ,(make-composed-keymap
                                           (cons aibo:clipboard-mode-map (nreverse maps))))))))

(add-hook 'aibo:chat-mode-hook #'aibo:--enable-clipboard-key)
(add-hook 'aibo:input-mode-hook #'aibo:--enable-clipboard-key)

(aibo:--define-keymap aibo:input-mode-map
                      (let ((map (make-sparse-keymap)))
                        (define-key map (kbd "RET") #'aibo:input-return)
                        (define-key map [remap next-line] #'aibo:input-next-line)
                        (define-key map [remap previous-line] #'aibo:input-previous-line)
                        (define-key map (kbd "M-RET") #'aibo:submit-input)
                        (define-key map (kbd "C-g") #'aibo:clear-input)
                        (define-key map (kbd "C-o") #'aibo:cycle-focus)
                        (define-key map (kbd "M-/") #'aibo:toggle-input-target)
                        (define-key map (kbd "C-c n") aibo:notification-map)
                        (define-key map (kbd "C-c C-i") #'aibo:attach-clipboard-image)
                        (define-key map (kbd "C-c b") #'aibo:manager-dispatch)

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

(dotimes (index 9)
  (let ((bot-index index)
        (key (kbd (format "M-%d" (1+ index)))))
    (dolist (map (list aibo:chat-mode-map aibo:input-mode-map))
      (define-key map key
                  (lambda () (interactive) (aibo:open-recent-bot bot-index))))))

(define-derived-mode aibo:input-mode fundamental-mode "Aibo-Input"
  "Major mode for the global Aibo input."
  (use-local-map aibo:input-mode-map)
  (setq-local mode-line-format nil)
  (setq-local header-line-format nil)
  (setq-local display-line-numbers nil)
  (when (fboundp 'display-line-numbers-mode) (display-line-numbers-mode -1))
  (when (bound-and-true-p linum-mode) (linum-mode -1))
  (setq-local word-wrap nil)
  (setq-local truncate-lines nil truncate-partial-width-windows nil)
  (setq-local electric-indent-inhibit t)
  (setq-local scroll-margin 0)
  (setq-local line-prefix " " wrap-prefix " ")
  (setq-local buffer-display-table (make-display-table))
  (set-display-table-slot buffer-display-table 'wrap (make-glyph-code ?\s))
  (add-to-invisibility-spec '(aibo-input-viewport))
  (add-hook 'pre-redisplay-functions #'aibo:--input-redisplay nil t)
  (cursor-intangible-mode 1)
  (add-hook 'after-change-functions #'aibo:--resize-input nil t)
  (add-hook 'post-command-hook #'aibo:--keep-point-in-input nil t))

(defun aibo:--click-map (command)
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] (lambda (_event) (interactive "e") (funcall command)))
    map))

(defun aibo:--select-input-target (chat)
  (setq aibo:input-target chat)
  (aibo:render-input)
  (when (window-live-p aibo:input-window) (select-window aibo:input-window))
  (aibo:--keep-point-in-input))

(defun aibo:--scroll-tabs (manager-p)
  (let* ((kind (if manager-p "manager" "bot"))
         (count (length (aibo:--recent-chats kind (if manager-p 10 9))))
         (symbol (if manager-p 'aibo:manager-tab-offset 'aibo:bot-tab-offset)))
    (set symbol (mod (1+ (symbol-value symbol)) (max 1 count)))
    (aibo:render-input)))

(defun aibo:--target-row (chat &optional key)
  (let* ((selected (equal (aibo:--get chat "id") (aibo:--get aibo:input-target "id")))
         (bot (equal (aibo:--get chat "kind") "bot"))
         (face (list (if selected 'aibo:selected-shortcut-face
                       (if bot 'aibo:bot-shortcut-face 'aibo:manager-shortcut-face))
                     (if selected (aibo:--badge-face chat)
                       (if bot 'aibo:purple-bar-face 'aibo:orange-bar-face))))
         (circle (if (or (aibo:--active-p chat)
                         (member (aibo:--get chat "status")
                                 '("interrupted" "cancelled" "error" "blocked")))
                     (concat (if (and (aibo:--active-p chat) (not selected))
                                 (propertize "●" 'face 'aibo:inactive-green-face)
                               (aibo:--status-circle chat)) " ") ""))
         (text (concat " " (truncate-string-to-width
                            (concat circle
                                    (or key (aibo:--chat-label chat)) " "
                                    (aibo:--chat-title chat)) 18 nil nil "…") " ")))
    (add-face-text-property 0 (length text) face t text)
    (add-text-properties 0 (length text)
                         `(keymap ,(aibo:--click-map (lambda () (aibo:--select-input-target chat)))
                                  mouse-face highlight) text)
    text))

(defun aibo:--target-rows (chats &optional manager-p)
  "Keep each tab's padding, giving adjacent titles a two-space gap."
  (let* ((offset (if manager-p aibo:manager-tab-offset aibo:bot-tab-offset))
         (index (+ offset (if manager-p 0 1))))
    (mapconcat
     (lambda (chat)
       (prog1 (aibo:--target-row chat (format (if manager-p "cb%d" "M-%d") index))
         (setq index (1+ index))))
     (nthcdr offset chats) "")))

(defun aibo:--input-redisplay (window)
  (when (eq window aibo:input-window)
    (let ((row (and aibo:input-chrome (overlay-get (nth 2 aibo:input-chrome) 'before-string))))
      (if (and row (/= (string-width row) (max 1 (1- (window-body-width window)))))
          (aibo:render-input)
        (aibo:--resize-input)))))

(defun aibo:--resize-input (&rest _ignored)
  "Fit visual draft lines while keeping control rows visible."
  (when (and (not aibo:input-rendering) aibo:input-start
             (window-live-p aibo:input-window)
             (eq (window-buffer aibo:input-window) (current-buffer)))
    (let* ((aibo:input-rendering t)
           (buffer-invisibility-spec nil)
           (window aibo:input-window)
           (lines (max 1 (count-screen-lines aibo:input-start (point-max) t window)))
           (chrome (1- (line-number-at-pos aibo:input-start)))
           (limit (max 1 (min 10 (- (/ (frame-height) 2) chrome))))
           (visible (min limit lines))
           (height (+ chrome visible))
           (row (max 0 (1- (count-screen-lines aibo:input-start (max aibo:input-start (point)) t window))))
           (offset (max 0 (- row visible -1)))
           (window-min-height 1))
      (unless aibo:input-viewport
        (setq aibo:input-viewport (list (make-overlay aibo:input-start aibo:input-start)
                                        (make-overlay (point-max) (point-max))))
        (dolist (overlay aibo:input-viewport)
          (overlay-put overlay 'invisible 'aibo-input-viewport)))
      (save-excursion
        (goto-char aibo:input-start)
        (vertical-motion offset window)
        (move-overlay (car aibo:input-viewport) aibo:input-start (point))
        (vertical-motion visible window)
        (move-overlay (cadr aibo:input-viewport) (point) (point-max)))
      (condition-case nil
          (window-resize window (- height (window-total-height window)))
        (error nil))
      (set-window-start window (point-min) t)
      (aibo:--edge-indicator 0 window
                             (and (> offset 0) (overlay-end (car aibo:input-viewport)))
                             "↑" 'aibo:muted-face 1))))

(defun aibo:input-next-line (count)
  (interactive "p")
  (let ((buffer-invisibility-spec nil)) (line-move count t))
  (aibo:--keep-point-in-input)
  (aibo:--resize-input))

(defun aibo:input-previous-line (count)
  (interactive "p")
  (aibo:input-next-line (- count)))

(defun aibo:--account-usage-badges (sidebar)
  "Format the Codex account badges for the global input row."
  (let* ((usage (aibo:--get sidebar "account_usage"))
         (remaining (aibo:--get usage "remaining_percent"))
         (resets (aibo:--get usage "resets")))
    (concat
     (propertize
      (aibo:--metadata-badge "usage" (if (numberp remaining) (format "%s%%" remaining) "—")
                             'aibo:account-label-face 'aibo:account-value-face)
      'help-echo "Remaining Codex usage (lowest quota window)")
     " "
     (propertize (aibo:--metadata-badge "resets" (if (numberp resets) resets "—")
                                        'aibo:account-label-face 'aibo:account-value-face)
                 'help-echo "Earned resets remaining"))))

(defun aibo:--input-status-row (width)
  "Keep the target on the left and account badges at the right of WIDTH cells."
  (let* ((badges (aibo:--account-usage-badges aibo:sidebar))
         (available (- width (string-width badges) 1))
         (target (if aibo:input-target
                     (concat " " (aibo:--badge aibo:input-target)
                             (propertize (format "  continue '%s'" (aibo:--chat-title aibo:input-target))
                                         'face 'aibo:muted-face))
                   (concat " " (propertize " m " 'face 'aibo:manager-badge-face)
                           (propertize "  New chat" 'face 'aibo:muted-face)))))
    (if (< available 1)
        (truncate-string-to-width badges width)
      (setq target (truncate-string-to-width target available nil nil "…"))
      (concat target (make-string (- width (string-width target) (string-width badges)) ?\s)
              badges))))

(defun aibo:render-input ()
  "Refresh display-only chrome without rewriting the draft or its undo history."
  (with-current-buffer (get-buffer-create aibo:input-buffer)
    (unless (derived-mode-p 'aibo:input-mode) (aibo:input-mode))
    (unless (and aibo:input-start aibo:input-chrome)
      ;; Upgrade an already open composer as well as initializing a new one.
      (let ((text (if aibo:input-start
                      (buffer-substring-no-properties aibo:input-start (point-max)) ""))
            (inhibit-read-only t)
            (inhibit-modification-hooks t))
        (erase-buffer)
        (insert (propertize "\n\n\n" 'read-only t 'cursor-intangible t
                            'rear-nonsticky t 'line-prefix "" 'wrap-prefix ""))
        (setq aibo:input-start (copy-marker (point)))
        (insert text)
        (setq aibo:rendered-attachments nil buffer-undo-list nil)
        (setq aibo:input-chrome
              (mapcar (lambda (pos) (make-overlay pos pos)) '(1 2 3)))))
    (let* ((width (if (window-live-p aibo:input-window)
                      (window-body-width aibo:input-window) (frame-width)))
           (rows (list
                  (concat "          " (aibo:--target-rows (aibo:--recent-chats "bot" 9)))
                  (concat (propertize " M-0 new  "
                                      'face (list 'aibo:manager-shortcut-face
                                                  (if aibo:input-target 'aibo:orange-bar-face
                                                    'aibo:manager-badge-face))
                                      'keymap (aibo:--click-map #'aibo:new-manager-chat) 'mouse-face 'highlight)
                          (aibo:--target-rows (aibo:--recent-chats "manager" 10) t))
                  (aibo:--input-status-row (max 1 (1- width))))))
      (cl-mapc (lambda (overlay row face)
                 (let ((text (truncate-string-to-width
                              row (max 1 (1- width)) nil ?\s
                              (when (memq face '(aibo:purple-bar-face aibo:orange-bar-face)) "…"))))
                   (when (and (memq face '(aibo:purple-bar-face aibo:orange-bar-face))
                              (> (string-width row) (1- width)))
                     (let ((manager-p (eq face 'aibo:orange-bar-face)))
                       (setq text (concat (substring text 0 -1)
                                          (propertize "›" 'face face
                                                      'keymap (aibo:--click-map (lambda () (aibo:--scroll-tabs manager-p)))
                                                      'mouse-face 'highlight)))))
                   (add-face-text-property 0 (length text) face t text)
                   (add-text-properties 0 (length text) '(line-prefix "" wrap-prefix "") text)
                   (overlay-put overlay 'before-string text)
                   (let ((inhibit-read-only t) (inhibit-modification-hooks t) (buffer-undo-list t))
                     (put-text-property (overlay-start overlay) (1+ (overlay-start overlay)) 'face face))))
               aibo:input-chrome rows
               '(aibo:purple-bar-face aibo:orange-bar-face aibo:base-face)))
    (setq-local header-line-format nil)
    (aibo:--sync-attachments)
    (when aibo:input-face-cookie
      (face-remap-remove-relative aibo:input-face-cookie))
    (setq aibo:input-face-cookie
          (face-remap-add-relative 'default
                                   (if (equal (aibo:--get aibo:input-target "kind") "bot")
                                       'aibo:bot-input-face 'aibo:manager-input-face)))
    (set-display-table-slot buffer-display-table 'wrap
                            (make-glyph-code ?\s
                                             (if (equal (aibo:--get aibo:input-target "kind") "bot")
                                                 'aibo:bot-input-face 'aibo:manager-input-face)))
    (aibo:--resize-input)))

(defun aibo:--sync-attachments ()
  "Update attachment buttons, shifting the draft's existing undo coordinates."
  (unless (equal aibo:rendered-attachments aibo:input-attachments)
    (let ((old-start (marker-position aibo:input-start))
          (offset (max 0 (- (point) aibo:input-start)))
          (history buffer-undo-list)
          (inhibit-read-only t)
          (inhibit-modification-hooks t))
      (let ((buffer-undo-list t))
        (delete-region aibo:input-prefix-end aibo:input-start)
        (goto-char aibo:input-prefix-end)
        (aibo:--insert-attachments)
        (add-text-properties aibo:input-prefix-end (point) '(read-only t rear-nonsticky t))
        (add-face-text-property aibo:input-prefix-end (point) '(:inherit aibo:base-face :extend t) t)
        (let ((prefix (propertize " " 'face 'aibo:base-face)))
          (add-text-properties aibo:input-prefix-end (point) `(line-prefix ,prefix wrap-prefix ,prefix)))
        (set-marker aibo:input-start (point)))
      (let ((delta (list (cons aibo:input-prefix-end (- old-start aibo:input-start)))))
        (when (listp history)
          (setq buffer-undo-list
                (mapcar (lambda (entry) (undo-adjust-elt entry delta)) history))))
      (setq aibo:rendered-attachments (copy-sequence aibo:input-attachments))
      (goto-char (min (point-max) (+ aibo:input-start offset))))))

(defun aibo:input-return ()
  (interactive)
  (if (button-at (point)) (push-button) (newline)))

(defun aibo:--keep-point-in-input ()
  (when (and aibo:input-start (< (point) aibo:input-prefix-end))
    (goto-char aibo:input-start))
  (aibo:--resize-input))

(defun aibo:focus-input (&optional new-chat)
  (interactive)
  (unless (derived-mode-p 'aibo:input-mode)
    (setq aibo:input-return-window (selected-window)))
  (setq aibo:input-target
        (unless new-chat (aibo:--input-source-chat)))
  (aibo:render-input)
  (if (and (eq aibo:layout 'full) (window-live-p aibo:input-window))
      (progn
        (set-window-buffer aibo:input-window (get-buffer aibo:input-buffer))
        (select-window aibo:input-window))
    (setq aibo:input-window
          (display-buffer-in-side-window
           (get-buffer aibo:input-buffer) '((side . bottom) (window-height . 4))))
    (select-window aibo:input-window))
  (aibo:--resize-input))

(defun aibo:--input-source-chat ()
  "Return the active conversation from which the composer was opened."
  (if (eq aibo:layout 'cowork)
      (when (window-live-p aibo:input-return-window)
        (buffer-local-value 'aibo:buffer-chat (window-buffer aibo:input-return-window)))
    aibo:current-chat))

(defun aibo:toggle-input-target ()
  "Toggle between the active conversation and a new manager draft."
  (interactive)
  (setq aibo:input-target (unless aibo:input-target (aibo:--input-source-chat)))
  (aibo:render-input))

(defun aibo:clear-input (&optional keep-open)
  (interactive)
  (with-current-buffer (get-buffer-create aibo:input-buffer)
    (when aibo:input-start
      (delete-region aibo:input-start (point-max))))
  (setq aibo:input-attachments nil)
  (aibo:render-input)
  (when (and (eq aibo:layout 'cowork) (not keep-open))
    (when-let ((window (get-buffer-window aibo:input-buffer)))
      (quit-window nil window))
    (when (window-live-p aibo:input-return-window)
      (select-window aibo:input-return-window)))
  (when (called-interactively-p 'interactive)
    (aibo:--echo (if (and (eq aibo:layout 'cowork) aibo:buffer-chat)
                     (format "Draft cleared · returned to %s" (aibo:--chat-label aibo:buffer-chat))
                   "Draft cleared"))))

(defun aibo:--echo (text &optional error-p)
  "Show transient TEXT without introducing persistent shortcut chrome."
  (message "%s" (propertize text 'face (if error-p 'aibo:error-message-face
                                         'aibo:echo-face))))

(defun aibo:--submit-to-chat (chat text attachments)
  (setq aibo:submitting chat)
  (aibo:--echo (format "Sending to %s…" (aibo:--chat-label chat)))
  (aibo:api-submit
   (aibo:--get chat "id") text attachments
   (lambda (response)
     (setq aibo:submitting nil)
     (let (cleared)
       (with-current-buffer aibo:input-buffer
         ;; A reply must never erase text typed while the request was in flight.
         (when (and (equal (aibo:--get aibo:input-target "id") (aibo:--get chat "id"))
                    (equal text (buffer-substring-no-properties aibo:input-start (point-max)))
                    (equal attachments aibo:input-attachments))
           (aibo:clear-input t)
           (setq cleared t)))
       (aibo:render-chat (aibo:--get response "chat") t)
       (aibo:--echo (format (if cleared "Message sent to %s"
                              "Previous message sent to %s · current draft preserved")
                            (aibo:--chat-label chat)))))
   (lambda (_error)
     (setq aibo:submitting nil)
     (aibo:--echo (format "Could not send to %s · reconnect and retry · draft preserved"
                          (aibo:--chat-label chat)) t))))

(defun aibo:set-goal (&optional disable)
  "Set the conversation goal to its last sent user message.
With a prefix argument DISABLE, turn goal mode off.  The draft is preserved."
  (interactive "P")
  (let ((chat (if (derived-mode-p 'aibo:input-mode)
                  aibo:input-target
                (or aibo:buffer-chat aibo:current-chat))))
    (unless chat (user-error "Open a conversation first"))
    (aibo:api-set-goal
     (aibo:--get chat "id") (not disable)
     (lambda (updated)
       (aibo:render-chat updated t)
       (aibo:--echo (if disable "Goal mode disabled" "Goal set to last sent message"))))))

(defun aibo:new-manager-chat ()
  "Focus a new manager draft; create the chat when it is sent."
  (interactive)
  (aibo:focus-input t))

(defun aibo:--create-manager-chat (on-created)
  (aibo:api-create-chat
   "manager"
   (lambda (chat)
     (push chat aibo:chats)
     (setq aibo:input-target chat)
     (aibo:render-chat chat)
     (funcall on-created chat))
   nil nil
   (lambda (_error)
     (setq aibo:submitting nil)
     (message "Aibo: could not create manager; your draft is preserved"))))

(defun aibo:submit-input ()
  (interactive)
  (if aibo:submitting
      (aibo:--echo (format "Sending to %s… · additional submit ignored"
                           (if (hash-table-p aibo:submitting)
                               (aibo:--chat-label aibo:submitting) "new chat")))
    (let ((text (buffer-substring-no-properties aibo:input-start (point-max)))
          (attachments (copy-sequence aibo:input-attachments)))
      (unless (and (string-blank-p text) (null attachments))
        (setq aibo:submitting t)
        (condition-case error
            (if aibo:input-target
                (aibo:--submit-to-chat aibo:input-target text attachments)
              (aibo:--create-manager-chat
               (lambda (chat) (aibo:--submit-to-chat chat text attachments))))
          (error (setq aibo:submitting nil)
                 (signal (car error) (cdr error))))))))

(provide 'aibo-ui-input)
;;; aibo-ui-input.el ends here
