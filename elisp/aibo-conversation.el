;;; aibo-conversation.el --- Methods related to conversations and the conversation buffer view -*- lexical-binding: t -*-
(require 'dash)
(require 'ht)
(require 'widget)

(require 'aibo-api)
(require 'aibo-types)

(defun aibo:--get-conversation-buffer-name (&rest args)
  (interactive)
  (let* ((id (plist-get args :id))
         (title (plist-get args :title))
         (short-id (replace-regexp-in-string "-" "" id)))
    (format "*Aibo [%.7s] - %s*" short-id title)))

(setq aibo:--message-role-colors
      (ht ("system-dark"     "#377047")
          ("system-light"    "#beface")
          ("user-dark"       "#ba7a13")
          ("user-light"      "#fae2b9")
          ("assistant-dark"  "#384a8f")
          ("assistant-light" "#dce9fc")
          ("error-dark"      "#8f3838")
          ("error-light"     "#fcdcdc")))

(setq aibo:--new-user-message-keymap
      (let ((keymap (copy-keymap widget-keymap)))
        (define-key keymap (kbd "RET") (lambda () (interactive) (insert "\n")))
        (define-key keymap (kbd "M-RET") #'aibo:submit-user-message)
        keymap))

(defun aibo::--get-role-color (role shade)
  (ht-get aibo:--message-role-colors (format "%s-%s" role shade) ))

(defun aibo::--get-message-color (message shade)
  (aibo::--get-role-color (oref message :role) shade))

(defun aibo::--get-message-header (message)
  (let ((role (oref message :role)))
    (cond
     ((string= role "system") "[ System ]")
     ((string= role "user") "[ User ]")
     ((string= role "assistant") (format "[ Assistant: %s ]" (aibo::--get-message-source message)))
     ((string= role "error") "[ Error ]"))))

(defun aibo::--get-message-source (message)
  (let* ((source (oref message :source))
         (source-kind (aibo:xref source :kind)))
    (cond
     ((string= source-kind "human") "User")
     ((string= source-kind "openai_model") (aibo:xref source :model))
     ((string= source-kind "programmatic") (aibo:xref source :source)))))

(defun aibo:refresh-current-conversation ()
  (interactive)
  (aibo:go-to-conversation-by-id (oref aibo:b-conversation :id)))

(defun aibo:go-to-conversation-by-id (conversation-id)
  (interactive)
  (aibo:api-get-conversation
   :id conversation-id
   :on-success (lambda (conversation)
                 (aibo:go-to-conversation conversation))))

(defun aibo:go-to-conversation (conversation)
  (interactive)
  (aibo:--get-or-create-buffer
   :name (aibo:--get-conversation-buffer-name
          :id (oref conversation :id)
          :title (oref conversation :title))
   :open-style :current-window
   :on-load
   (lambda (buffer)
     (let* ((set-not-modified (not (buffer-modified-p)))
            (original-point (if (> (buffer-size) 0) (point) nil))
            (original-window-start (window-start))
            (point-was-inside-user-widget?
             (and original-point
                  aibo:b-new-user-message-widget
                  (>= original-point (widget-field-start aibo:b-new-user-message-widget))
                  (<= original-point (widget-field-end aibo:b-new-user-message-widget)))))

       ;; Kill all the widget local variables but
       ;; remember to keep our own local varibles
       (kill-all-local-variables)
       (remove-overlays)
       (let ((inhibit-read-only t))
         (erase-buffer))

       (aibo:conversation-mode)

       ;; All the buffer-specific variables should be defined here (even if set to nil by default)
       (setq-local aibo:b-conversation conversation)
       (setq-local aibo:b-message-widgets (ht-create))
       (setq-local aibo:b-streaming-assistant-message-widget nil)
       (setq-local aibo:b-new-user-message-widget nil)

       ;; Render header
       (setq header-line-format
             (format "[%s] %s"
                     aibo:model-name
                     (oref conversation :title)))

       ;; Render title
       (widget-insert (propertize
                       (format (concat "ID: %s\n"
                                       "--------------------------------------------------\n\n")
                               (oref conversation :id)
                               (oref conversation :title))
                       'font-lock-face '(:foreground "#9c9c9c")))

       ;; Render conversation
       (aibo:--render-conversation conversation)

       ;; Render streamed assistant completion
       (setq-local aibo:b-streaming-assistant-message-widget
                   (widget-create 'chat-message :value nil))

       ;; Render User input
       (widget-insert (propertize
                       "[ User ]"
                       'font-lock-face `(:background ,(aibo::--get-role-color "user" "dark"))))
       (widget-insert "\n")
       (setq-local aibo:b-new-user-message-widget
                   (widget-create 'text
                                  :format "%v"
                                  :button-suffix ""
                                  :keymap aibo:--new-user-message-keymap))

       ;; Clean up buffer state
       (if set-not-modified
           (set-buffer-modified-p nil))

       (widget-setup)

       (if (or point-was-inside-user-widget? (not original-point))
           (progn
             (goto-char (widget-field-start aibo:b-new-user-message-widget))
             (let ((recenter-positions '(bottom)))
               (recenter-top-bottom)))
         (progn
           (goto-char original-point)
           (set-window-start nil original-window-start)))))))

(defun aibo:set-current-conversation-title ()
  (interactive)
  (aibo:set-conversation-title aibo:b-conversation))

(defun aibo:generate-current-conversation-title ()
  (interactive)
  (aibo:api-generate-conversation-title
   :id (oref aibo:b-conversation :id)
   :on-success
   (lambda (updated-conversation)
     (aibo:--on-conversation-title-change
      :prev-conversation aibo:b-conversation
      :conversation updated-conversation))))

(defun aibo:set-conversation-title (conversation)
  (interactive)
  (let* ((id (oref conversation :id))
         (title (read-string "Title: ")))
    (aibo:api-set-conversation-title
     :id id
     :title title
     :on-success
     (lambda (updated-conversation)
       (aibo:--on-conversation-title-change
        :prev-conversation conversation
        :conversation updated-conversation)))))

(defun aibo:--on-conversation-title-change (&rest args)
  (let* ((prev-conversation (plist-get args :prev-conversation))
         (conversation (plist-get args :conversation))
         (prev-buffer-name (aibo:--get-conversation-buffer-name
                            :id (oref prev-conversation :id)
                            :title (oref prev-conversation :title)))
         (prev-buffer (get-buffer prev-buffer-name)))
    (save-window-excursion
      (aibo:homepage))

    (if (string= prev-buffer-name (buffer-name))
        (aibo:go-to-conversation conversation))

    (if prev-buffer
        (kill-buffer prev-buffer))))

(defun aibo:submit-user-message ()
  (interactive)
  (let* ((text (widget-value aibo:b-new-user-message-widget)))
    (widget-value-set aibo:b-new-user-message-widget "")
    (aibo:api-submit-user-message
     :conversation-id (oref aibo:b-conversation :id)
     :text text
     :on-success
     (lambda (conversation)
       (aibo:go-to-conversation conversation)
       (aibo:stream-assistant-message)))))

(defun aibo:stream-assistant-message (&rest args)
  (interactive)
  (let* ((on-success (plist-get args :on-success)))
    (aibo:api-ws-stream-assistant-message
     :conversation-id (oref aibo:b-conversation :id)
     :on-message (lambda (message)
                   (widget-value-set
                    aibo:b-streaming-assistant-message-widget
                    message)
                   (set-buffer-modified-p nil))
     :on-success (lambda ()
                   (aibo:refresh-current-conversation)
                   (if on-success (funcall on-success))))))

;; ---[ Render conversation ]---------------------
(define-widget 'chat-message 'default
  "Displays the chat message"
  :format "%v"
  :value-create 'aibo:--chat-message-widget-value-create
  :value-to-internal 'aibo:--chat-message-widget-value-to-internal)

(defun aibo:--chat-message-widget-value-create (widget)
  (widget-insert (widget-get widget :value)))

(defun aibo:--chat-message-widget-value-to-internal (widget message)
  (if message
      (let* ((message-header-color (aibo::--get-message-color message "dark"))
             (message-content-color (aibo::--get-message-color message "light"))
             (message-suffix (if (string= (oref message :status) "streaming") "█" "")))
        (concat
         (propertize
          (aibo::--get-message-header message)
          'read-only t
          'message message
          'font-lock-face `(:background ,message-header-color))
         (propertize
          (format "\n%s%s" (oref message :content-text) message-suffix)
          'read-only t
          'message message
          'font-lock-face `(:foreground ,message-content-color))
         (propertize
          "\n\n"
          'read-only t
          'message message)))
    "\n"))

(defun aibo:--render-message-widget (message)
  (let* ((message-widget (widget-create 'chat-message :value message)))
    (ht-set! aibo:b-message-widgets
             (oref message :id)
             message-widget)))

(defun aibo:--update-message-widget (message widget)
  (let* ((message-widget
          (ht-get aibo:b-message-widgets (oref message :id))))
    (widget-value-set message-widget message)))

(defun aibo:--render-conversation (conversation)
  (let ((all-messages (oref conversation :all-messages))
        (rendered-messages nil)
        (current-message-id (oref conversation :current-message-id)))

    (while current-message-id
      (setq message (cdr (assoc (intern current-message-id) all-messages)))

      (if message
          (progn
            (push message rendered-messages)
            (setq current-message-id (oref message :parent-id)))
        (setq current-message-id nil)))

    (--each rendered-messages (aibo:--render-message-widget it))))


;; ---[ Conversation editing ]--------------------
(defun aibo:--message-at-point ()
  (interactive)
  (get-char-property (point) 'message))

(defun aibo:--widget-at-point ()
  (interactive)
  (let* ((message (aibo:--message-at-point)))
    (ht-get aibo:b-message-widgets (oref message :id))))

(defun aibo:remove-message-at-point ()
  (interactive)
  (let* ((message (aibo:--message-at-point))
         (message-id (oref message :id)))
    (aibo:api-delete-message
     :conversation-id (oref aibo:b-conversation :id)
     :message-id message-id
     :delete-after nil
     :on-success (lambda (conversation)
                   (aibo:go-to-conversation conversation)))))

(defun aibo:remove-messages-after-point ()
  (interactive)
  (let* ((message (aibo:--message-at-point))
         (message-id (oref message :id)))
    (aibo:api-delete-message
     :conversation-id (oref aibo:b-conversation :id)
     :message-id message-id
     :delete-after t
     :on-success (lambda (conversation)
                   (aibo:go-to-conversation conversation)))))

;; TODO
(defun aibo:edit-message-at-point ()
  (interactive)
  (let* ((message (aibo:--message-at-point))
         (message-id (oref message :id)))))

(defun aibo:regenerate-last-message ()
  (interactive)
  (aibo:api-regenerate-assistant-message
   :conversation-id (oref aibo:b-conversation :id)
   :on-success (lambda (conversation)
                 (aibo:go-to-conversation conversation))))

;; ---[ Create a conversation ]-------------------
(defvar aibo:--create-conversation-history nil
  "History for `aibo:create-conversation'")

(defun aibo:--conversation-template-options ()
  (--map
   (format "%-10s%s" (oref it :short-name) (oref it :name))
   aibo:conversation-templates))

(defun aibo:create-conversation ()
  (interactive)
  (ivy-read "Template: " (aibo:--conversation-template-options)
            :require-match t
            :history 'aibo:--create-conversation-history
            :action #'aibo:--create-conversation-from-template
            :caller 'aibo:--create-conversation-with-content))

(ivy-configure 'aibo:--create-conversation-with-content
  :initial-input "^")

(defun aibo:--create-conversation-from-template (template-option)
  (let* ((template-short-name (car (split-string template-option "\\s-+")))
         (template (aibo:get-conversation-template
                    :short-name template-short-name))
         (content-input (read-string (format "%s: " (oref template :name))))
         (content (if (string= content-input "")
                      (buffer-substring (region-beginning) (region-end))
                    content-input))
         (message-inputs (aibo:get-conversation-template-message-inputs
                          :template template
                          :content content))
         (api-message-inputs (--map
                              (CreateMessageInput-to-api it)
                              message-inputs)))

    (aibo:api-create-conversation
     :message-inputs api-message-inputs
     :on-success
     (lambda (conversation)
       (aibo:go-to-conversation conversation)
       (aibo:stream-assistant-message
        :on-success (lambda ()
                      (aibo:generate-current-conversation-title)))))))

(provide 'aibo-conversation)
