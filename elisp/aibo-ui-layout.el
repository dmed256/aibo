;;; aibo-ui-layout.el --- Window layout and navigation -*- lexical-binding: t -*-

(require 'aibo-ui-base)
(declare-function aibo:search-chats "aibo-search")
(declare-function aibo:chat-mode "aibo-ui-chat")
(declare-function aibo:sidebar-mode "aibo-ui-chat")
(declare-function aibo:open-chat-id "aibo-ui-chat")
(declare-function aibo:render-input "aibo-ui-input")

(defun aibo:--edge-indicator (slot window position character face &optional prefix)
  "Paint CHARACTER in the last usable cell without changing buffer text."
  (dolist (entry aibo:edge-indicators)
    (unless (and (window-live-p (car entry))
                 (eq (window-buffer (car entry)) (current-buffer)))
      (mapc #'delete-overlay (cdr entry))
      (setq aibo:edge-indicators (delq entry aibo:edge-indicators))))
  (unless (assq window aibo:edge-indicators)
    (push (list window (make-overlay (point-min) (point-min))
                (make-overlay (point-min) (point-min))) aibo:edge-indicators))
  (let ((overlay (nth slot (cdr (assq window aibo:edge-indicators)))))
    (overlay-put overlay 'window window)
    (overlay-put overlay 'before-string nil)
    (overlay-put overlay 'display nil)
    (move-overlay overlay (point-min) (point-min))
    (when position
      (save-excursion
        (goto-char position)
        (let* ((column (- (window-body-width window) 2))
               (text (buffer-substring position (line-end-position)))
               (prefix-text (truncate-string-to-width text (- column (or prefix 0))))
               (glyph (propertize character 'face (list face 'aibo:base-face))))
          (goto-char (+ position (length prefix-text)))
          (if (< (point) (line-end-position))
              (progn
                (move-overlay overlay (point) (1+ (point)))
                (overlay-put overlay 'display glyph))
            (move-overlay overlay (point) (point))
            (overlay-put overlay 'before-string
                         (concat (make-string (max 0 (- column (or prefix 0) (string-width prefix-text))) ?\s) glyph))))))))

(defun aibo:--page-redisplay (window)
  (unless (derived-mode-p 'aibo:sidebar-mode 'aibo:search-mode)
    (let* ((start (window-start window))
           (height (window-body-height window))
           (bottom (when (> (count-screen-lines start (point-max) nil window) height)
                     (save-excursion (goto-char start) (vertical-motion (1- height) window) (point)))))
      (aibo:--edge-indicator 0 window (and (not aibo:buffer-chat) (> start (point-min)) start) "↑" 'aibo:muted-face
                             (string-width (or (get-text-property start 'wrap-prefix) wrap-prefix "")))
      (aibo:--edge-indicator 1 window bottom "↓" 'aibo:muted-face
                             (when bottom (string-width (or (get-text-property bottom 'wrap-prefix) wrap-prefix "")))))))

(defun aibo:--sidebar-redisplay (window)
  "Keep notification shortcuts anchored at the top, without overflow arrows."
  (dolist (entry aibo:edge-indicators)
    (mapc #'delete-overlay (cdr entry)))
  (setq aibo:edge-indicators nil)
  (set-window-start window (point-min)))

(defun aibo:cycle-focus ()
  (interactive)
  (if (eq aibo:layout 'full)
      (let ((destination (if (eq (selected-window) aibo:input-window)
                             aibo:main-window
                           aibo:input-window)))
        (when (window-live-p destination)
          (select-window destination)))
    (let* ((windows (seq-filter
                     (lambda (window) (not (window-parameter window 'no-other-window)))
                     (window-list nil 'no-minibuffer)))
           (next (cadr windows)))
      (when next (select-window next)))))

(defun aibo:focus-sidebar ()
  (interactive)
  (unless (and (eq aibo:layout 'full) (window-live-p aibo:sidebar-window))
    (user-error "Notifications are available in full mode"))
  (select-window aibo:sidebar-window))

(defun aibo:--full-layout (main-buffer)
  "Reuse the three full-mode windows; create them only when entering full mode."
  (when (eq aibo:layout 'full)
    (unless (and (window-live-p aibo:main-window)
                 (window-live-p aibo:sidebar-window)
                 (window-live-p aibo:input-window)
                 (not (window-dedicated-p aibo:main-window)))
      (when (window-dedicated-p (selected-window))
        (select-window (seq-find (lambda (window) (not (window-dedicated-p window)))
                                 (window-list nil 'no-minibuffer))))
      (delete-other-windows)
      (setq aibo:main-window (selected-window))
      (let ((window-min-height 1)
            (window-min-width 10))
        (setq aibo:input-window (split-window aibo:main-window -5 'below))
        (setq aibo:sidebar-window
              (split-window aibo:main-window
                            (- (min (1+ aibo:sidebar-width)
                                    (max 10 (- (window-total-width aibo:main-window) 30))))
                            'left)))
      (set-window-buffer aibo:sidebar-window (get-buffer-create aibo:sidebar-buffer))
      (set-window-buffer aibo:input-window (get-buffer-create aibo:input-buffer))
      (set-window-dedicated-p aibo:sidebar-window t)
      (window-preserve-size aibo:sidebar-window t t)
      (set-window-dedicated-p aibo:input-window t)
      (set-window-parameter aibo:sidebar-window 'no-other-window t)
      (dolist (window (list aibo:main-window aibo:sidebar-window aibo:input-window))
        (set-window-fringes window 0 0)
        (set-window-margins window
                            (if (eq window aibo:main-window) 1 0)
                            0)
        (set-window-scroll-bars window nil nil)
        (set-window-display-table window
                                  (buffer-local-value 'buffer-display-table (window-buffer window)))))
    (let ((width (min (1+ aibo:sidebar-width)
                      (max 10 (- (frame-width (window-frame aibo:main-window)) 30)))))
      (unless (= width (window-total-width aibo:sidebar-window))
        (window-preserve-size aibo:sidebar-window t nil)
        (window-resize aibo:sidebar-window (- width (window-total-width aibo:sidebar-window)) t)
        (window-preserve-size aibo:sidebar-window t t)))
    (set-window-buffer aibo:main-window main-buffer)
    (aibo:render-input)))

(defun aibo:--recent-chats (kind limit)
  "List KIND chats, active first, then latest user or final-answer time."
  (let ((chats (seq-uniq (append aibo:chats (aibo:--get aibo:sidebar "active"))
                         (lambda (left right)
                           (equal (aibo:--get left "id") (aibo:--get right "id"))))))
    (seq-take
     (sort (seq-filter (lambda (chat) (string= (aibo:--get chat "kind") kind)) chats)
           (lambda (left right)
             (let ((left-active (and (aibo:--get left "active") t))
                   (right-active (and (aibo:--get right "active") t))
                   (left-time (or (aibo:--get left "last_active_at") (aibo:--get left "created_at")))
                   (right-time (or (aibo:--get right "last_active_at") (aibo:--get right "created_at"))))
               (if (not (eq left-active right-active)) left-active
                 (and left-time
                      (or (not right-time)
                          (time-less-p (date-to-time right-time)
                                       (date-to-time left-time))))))))
     limit)))

(defun aibo:open-recent-bot (index)
  (let ((chat (nth index (aibo:--recent-chats "bot" 9))))
    (unless chat (user-error "No bot chat at that index"))
    (aibo:open-chat-id (aibo:--get chat "id") t)))

(defun aibo:open-recent-manager (index)
  (let ((chat (nth index (aibo:--recent-chats "manager" 10))))
    (unless chat (user-error "No manager chat at that index"))
    (aibo:open-chat-id (aibo:--get chat "id") t)))

(defun aibo:manager-dispatch ()
  (interactive)
  (if (eq aibo:layout 'cowork)
      (aibo:search-chats)
    (let ((key (read-key "Manager: 0–9: ")))
      (unless (and (integerp key) (>= key ?0) (<= key ?9))
        (user-error "Manager index must be 0 through 9"))
      (aibo:open-recent-manager (- key ?0)))))

(defun aibo:--leave-full-layout ()
  "Enter cowork mode, hiding full-mode chrome without discarding drafts."
  (when (eq aibo:layout 'full)
    (when (window-live-p aibo:main-window) (select-window aibo:main-window))
    (setq aibo:layout 'cowork)
    (dolist (window (list aibo:sidebar-window aibo:input-window))
      (when (window-live-p window)
        (set-window-dedicated-p window nil)
        (delete-window window)))
    (setq aibo:sidebar-window nil aibo:input-window nil aibo:main-window nil)))

(defun aibo:--show-file-beside (source file)
  "Show SOURCE on the left and select FILE on the right in cowork mode."
  (aibo:--leave-full-layout)
  (let* ((left (or (get-buffer-window source) (selected-window)))
         (right (window-in-direction 'right left)))
    (select-window left)
    (set-window-buffer left source)
    ;; Reuse an existing two-pane layout when following another link.
    (unless (and (= (length (window-list)) 2) right
                 (not (window-dedicated-p right)))
      (delete-other-windows left)
      (setq right (split-window-right (1+ (/ (window-total-width) 2)))))
    (select-window right)
    (switch-to-buffer file)
    (aibo:--cowork-dividers)))

(defun aibo:--window-command (command)
  (aibo:--leave-full-layout)
  ;; C-x 0 still exits full mode even if it leaves one ordinary window.
  (unless (and (eq command #'delete-window) (one-window-p))
    (if (eq command #'split-window-right)
        ;; Share the content area, leaving the divider in its own cell.
        (split-window-right (1+ (/ (window-total-width) 2)))
      (call-interactively command)))
  (aibo:--cowork-dividers))

(defun aibo:--cowork-dividers (&rest _)
  "Give vertically stacked cowork windows a display-only separator."
  (dolist (window (window-list nil 'no-minibuffer))
    (with-current-buffer (window-buffer window)
      (when (derived-mode-p 'aibo:chat-mode)
        (let* ((edges (window-edges window))
               (above (and (eq aibo:layout 'cowork) (> (nth 1 edges) 0)
                           (window-at (car edges) (1- (nth 1 edges))))))
          (set-window-parameter window 'header-line-format
                                (when (and above (not aibo:buffer-chat))
                                  '(:eval (propertize (make-string (window-total-width) ?─)
                                                      'face 'aibo:rule-face)))))))))

(defun aibo:cowork-delete-window ()
  (interactive)
  (aibo:--window-command #'delete-window))

(defun aibo:cowork-delete-other-windows ()
  (interactive)
  (aibo:--window-command #'delete-other-windows))

(defun aibo:cowork-split-below ()
  (interactive)
  (aibo:--window-command #'split-window-below))

(defun aibo:cowork-split-right ()
  (interactive)
  (aibo:--window-command #'split-window-right))

(provide 'aibo-ui-layout)
;;; aibo-ui-layout.el ends here
