;;; aibo-search.el --- Search query and paged results -*- lexical-binding: t -*-
(require 'aibo-api)
(require 'aibo-custom)
(require 'cl-lib)
(require 'seq)

(defvar aibo:page)
(defvar aibo:layout)
(defvar aibo:main-window)
(declare-function aibo:chat-mode "aibo-ui-chat")
(declare-function aibo:--badge "aibo-ui-base")
(declare-function aibo:--title-text "aibo-ui-base")
(declare-function aibo:--full-layout "aibo-ui-layout")
(declare-function aibo:open-chat-id "aibo-ui-chat")

(defvar-local aibo:search-results nil)
(defvar-local aibo:search-more nil)
(defvar-local aibo:search-selection -1)
(defvar-local aibo:search-offset 0)
(defvar-local aibo:search-query "")
(defvar-local aibo:search-generation 0)
(defvar-local aibo:search-timer nil)
(defvar-local aibo:search-loading nil)
(defvar-local aibo:search-error nil)
(defvar-local aibo:search-body nil)

(aibo:--define-keymap aibo:search-mode-map
                      (let ((map (make-sparse-keymap)))
                        (define-key map (kbd "RET") #'aibo:search-activate)
                        (dolist (key '("<down>" "C-n" "TAB")) (define-key map (kbd key) #'aibo:search-next))
                        (dolist (key '("<up>" "C-p" "<backtab>")) (define-key map (kbd key) #'aibo:search-previous))
                        map))

(define-derived-mode aibo:search-mode aibo:chat-mode "Aibo-Search"
  "Edit a single query or select a search result."
  (setq-local buffer-read-only nil)
  (setq-local left-margin-width 0 right-margin-width 0)
  (setq-local truncate-lines t)
  (setq-local line-prefix " " wrap-prefix " ")
  (add-hook 'after-change-functions #'aibo:search-changed nil t)
  (add-hook 'post-command-hook #'aibo:search-constrain nil t)
  (add-hook 'window-size-change-functions #'aibo:search-resize nil t))

(defun aibo:search-resize (window)
  (with-current-buffer (window-buffer window)
    (when aibo:search-body (aibo:search-render))))

(defun aibo:search-count ()
  (+ (length aibo:search-results) (if aibo:search-more 1 0)))

(defun aibo:search-height ()
  (max 1 (- (if-let ((window (get-buffer-window))) (window-total-height window) 24) 2)))

(defun aibo:search-render ()
  "Update results without replacing the query or its undo history."
  (let* ((inhibit-read-only t) (inhibit-modification-hooks t) (buffer-undo-list t)
         (query-point (min (point) (1+ (length aibo:search-query))))
         (height (aibo:search-height))
         (width (- (if-let ((window (get-buffer-window))) (window-body-width window) (frame-width)) 2)))
    (unless aibo:search-body
      (erase-buffer)
      (insert aibo:search-query "\n\n")
      (setq aibo:search-body (copy-marker (point))))
    (save-excursion
      (goto-char (point-min))
      (let ((end (line-end-position)))
        (put-text-property (point-min) (1+ end) 'face 'aibo:search-input-face)
        (put-text-property (point-min) (1+ end) 'line-prefix
                           (propertize " " 'face 'aibo:search-input-face))
        (add-text-properties end (+ end 2) '(read-only t rear-nonsticky t)))
      (delete-region aibo:search-body (point-max))
      (goto-char aibo:search-body)
      (setq aibo:search-offset (max 0 (min aibo:search-offset (- (aibo:search-count) height))))
      (cl-loop for index from aibo:search-offset below (min (aibo:search-count) (+ aibo:search-offset height)) do
               (let* ((start (point)) (chat (nth index aibo:search-results))
                      (selected (= index aibo:search-selection)))
                 (insert (if chat
                             (truncate-string-to-width
                              (concat (aibo:--badge chat t) "  "
                                      (aibo:--title-text chat (max 1 (- width 2 (string-width (aibo:--badge chat t))))))
                              width nil nil "…")
                           "[Show 50 more]") "\n")
                 (add-text-properties start (point)
                                      `(read-only t front-sticky (read-only) rear-nonsticky t aibo-search-index ,index
                                                  mouse-face highlight))
                 (when selected
                   (add-face-text-property start (point) 'aibo:selected-face t)
                   (put-text-property start (point) 'line-prefix
                                      (propertize " " 'face 'aibo:selected-face)))))
      (when (= (aibo:search-count) 0)
        (insert (propertize (concat (cond (aibo:search-loading "Searching…")
                                          (aibo:search-error "Search failed. RET to retry.")
                                          (t "No matching chats.")) "\n")
                            'read-only t 'face 'aibo:muted-face))))
    (goto-char (if (< aibo:search-selection 0) query-point aibo:search-body))
    (when (>= aibo:search-selection 0)
      (forward-line (- aibo:search-selection aibo:search-offset)))
    (when-let ((window (get-buffer-window)))
      (set-window-start window (point-min) t))))

(defun aibo:search-move (delta)
  (setq aibo:search-selection (max -1 (min (+ aibo:search-selection delta) (1- (aibo:search-count)))))
  (when (>= aibo:search-selection 0)
    (setq aibo:search-offset
          (max 0 (min aibo:search-selection
                      (max aibo:search-offset (- (1+ aibo:search-selection) (aibo:search-height)))))))
  (aibo:search-render))

(defun aibo:search-next () (interactive) (aibo:search-move 1))
(defun aibo:search-previous () (interactive) (aibo:search-move -1))

(defun aibo:search-constrain ()
  "Keep point in the query or on a selectable result, including mouse moves."
  (let ((previous aibo:search-selection))
    (cond ((<= (point) (save-excursion (goto-char (point-min)) (line-end-position)))
           (setq aibo:search-selection -1))
          ((get-text-property (point) 'aibo-search-index)
           (setq aibo:search-selection (get-text-property (point) 'aibo-search-index))
           (beginning-of-line))
          (t (goto-char (point-min)) (end-of-line) (setq aibo:search-selection -1)))
    (unless (= previous aibo:search-selection) (aibo:search-render))))

(defun aibo:search-fetch (&optional more)
  (when (timerp aibo:search-timer) (cancel-timer aibo:search-timer))
  (setq aibo:search-timer nil aibo:search-loading t aibo:search-error nil)
  (let ((buffer (current-buffer))
        (generation (cl-incf aibo:search-generation))
        (query aibo:search-query)
        (offset (if more (length aibo:search-results) 0)))
    (unless more (setq aibo:search-results nil aibo:search-more nil))
    (aibo:search-render)
    (aibo:api-get-chats
     (lambda (chats)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (= generation aibo:search-generation)
             (setq aibo:search-results (append (and more aibo:search-results) (seq-take chats 50))
                   aibo:search-more (> (length chats) 50)
                   aibo:search-loading nil)
             (aibo:search-render)))))
     query 51 offset
     (lambda (_error)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (= generation aibo:search-generation)
             (setq aibo:search-loading nil aibo:search-error t)
             (aibo:search-render))))))))

(defun aibo:search-changed (&rest _)
  (setq aibo:search-query (buffer-substring-no-properties (point-min) (save-excursion (goto-char (point-min)) (line-end-position)))
        aibo:search-selection -1 aibo:search-offset 0)
  ;; Invalidate the old response immediately, before the debounce fires.
  (cl-incf aibo:search-generation)
  (when (timerp aibo:search-timer) (cancel-timer aibo:search-timer))
  (let ((buffer (current-buffer)))
    (setq aibo:search-timer
          (run-at-time 0.15 nil (lambda () (when (buffer-live-p buffer)
                                             (with-current-buffer buffer (aibo:search-fetch))))))))

(defun aibo:search-activate ()
  (interactive)
  (cond ((and aibo:search-more (= aibo:search-selection (length aibo:search-results)))
         (unless aibo:search-loading (aibo:search-fetch t)))
        ((nth (max 0 aibo:search-selection) aibo:search-results)
         (aibo:open-chat-id (gethash "id" (nth (max 0 aibo:search-selection) aibo:search-results))))
        (aibo:search-error (aibo:search-fetch))))

(defun aibo:search-chats ()
  (interactive)
  (setq aibo:page 'search)
  (let ((buffer (get-buffer-create "*Aibo search*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'aibo:search-mode) (aibo:search-mode)))
    (if (eq aibo:layout 'full)
        (progn (aibo:--full-layout buffer) (select-window aibo:main-window)
               (set-window-margins aibo:main-window 0 0))
      (switch-to-buffer buffer))
    (aibo:search-fetch)))

(provide 'aibo-search)
