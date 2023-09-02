;;; aibo-conversation.el --- Methods related to conversations and the conversation buffer view -*- lexical-binding: t -*-
(require 'dash)
(require 'ht)
(require 'widget)

(require 'aibo-api)
(require 'aibo-types)

(defun aibo:--get-conversation-buffer-name (id)
  (format "*Aibo [%s]*" id))

(defun aibo:--get-conversation-buffer (id)
  (get-buffer (aibo:--get-conversation-buffer-name id)))

(setq aibo:--new-user-message-keymap
      (let ((map (copy-keymap widget-keymap)))
        (define-key map (kbd "C-c C-x C-r") #'aibo:refresh-current-conversation)
        (define-key map (kbd "C-c C-t")     #'aibo:set-current-conversation-title)
        (define-key map (kbd "C-c C-c")     #'aibo:regenerate-current-conversation-last-assistant-message)
        (define-key map (kbd "C-c C-x C-t") #'aibo:generate-current-conversation-title)
        (define-key map (kbd "RET")         (lambda () (interactive) (insert "\n")))
        (define-key map (kbd "M-RET")       #'aibo:submit-user-message)
        map))

(defun aibo::--get-role-color (role shade)
  (ht-get aibo:message-role-colors (format "%s-%s" role shade) ))

(defun aibo::--get-message-color (message shade)
  (aibo::--get-role-color (ht-get message "role") shade))

(defun aibo::--get-message-header (message)
  (let ((role (ht-get message "role")))
    (cond
     ((string= role "system")    "[ System ]")
     ((string= role "user")      "[ User ]")
     ((string= role "assistant") (format "[ Assistant: %s ]" (aibo::--get-message-source message)))
     ((string= role "error")     "[ Error ]"))))

(defun aibo::--get-message-source (message)
  (let* ((source (ht-get message "source"))
         (source-kind (ht-get source "kind")))
    (cond
     ((string= source-kind "human")        "User")
     ((string= source-kind "openai_model") (ht-get source "model"))
     ((string= source-kind "programmatic") (ht-get source "source")))))

(defun aibo:refresh-current-conversation ()
  (interactive)
  (aibo:go-to-conversation-by-id
   :conversation-id (ht-get aibo:b-conversation "id")
   :sync t))

(defun aibo:go-to-conversation (&rest args)
  "Load the conversation in its buffer"
  (interactive)
  (let* ((conversation (plist-get args :conversation))
         (buffer-open-style (or (plist-get args :buffer-open-style) :current-window)))
    (aibo:--get-or-create-buffer
     :name (aibo:--get-conversation-buffer-name (ht-get conversation "id"))
     :open-style buffer-open-style
     :on-load
     (lambda (buffer)
       (with-current-buffer buffer
         (aibo:--dangerously-render-conversation conversation))))))

(defun aibo:go-to-conversation-by-id (&rest args)
  "Go to the conversation buffer, but if it doesn't exist reload"
  (interactive)
  (let* ((conversation-id (plist-get args :conversation-id))
         (buffer-open-style (or (plist-get args :buffer-open-style) :current-window))
         (sync (plist-get args :sync)))
    (aibo:--get-or-create-buffer
     :name (aibo:--get-conversation-buffer-name conversation-id)
     :open-style buffer-open-style
     :on-load
     (lambda (buffer)
       (if sync
           (aibo:api-get-conversation
            :id conversation-id
            :on-success (lambda (conversation)
                          (with-current-buffer buffer
                            (aibo:--dangerously-render-conversation conversation))))))
     :on-create
     (lambda (buffer)
       (aibo:api-get-conversation
        :id conversation-id
        :on-success (lambda (conversation)
                      (with-current-buffer buffer
                        (aibo:--dangerously-render-conversation conversation))))))))

(defun aibo:--dangerously-render-conversation (conversation)
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
                  aibo:model
                  (ht-get conversation "title")))

    ;; Render title
    (widget-insert (propertize
                    (format (concat "ID: %s\n"
                                    "--------------------------------------------------\n\n")
                            (ht-get conversation "id")
                            (ht-get conversation "title"))
                    'font-lock-face `(:foreground ,aibo:conversation-header-foreground-color)))

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
        (set-window-start nil original-window-start)))))

(defun aibo:set-current-conversation-title ()
  (interactive)
  (aibo:set-conversation-title aibo:b-conversation))

(defun aibo:generate-current-conversation-title ()
  (interactive)
  (aibo:api-generate-conversation-title
   :id (ht-get aibo:b-conversation "id")
   :on-success
   (lambda (updated-conversation)
     (save-window-excursion (aibo:homepage)))))

(defun aibo:set-conversation-title (conversation)
  (interactive)
  (let* ((id (ht-get conversation "id"))
         (title (read-string "Title: ")))
    (aibo:api-set-conversation-title
     :id id
     :title title
     :on-success
     (lambda (updated-conversation)
       (save-window-excursion (aibo:homepage))))))

(defun aibo:submit-user-message ()
  (interactive)
  (let* ((text (widget-value aibo:b-new-user-message-widget))
         (conversation-id (ht-get aibo:b-conversation "id")))
    (widget-value-set aibo:b-new-user-message-widget "")
    (aibo:api-submit-user-message
     :conversation-id conversation-id
     :text text
     :on-success
     (lambda (conversation)
       (aibo:go-to-conversation :conversation conversation)
       (aibo:stream-assistant-message
        :conversation-id conversation-id)))))

(defun aibo:stream-assistant-message (&rest args)
  (interactive)
  (let* ((conversation-id (plist-get args :conversation-id))
         (on-success (plist-get args :on-success))
         (buffer (aibo:--get-conversation-buffer conversation-id)))
    (aibo:api-ws-stream-assistant-message
     :conversation-id conversation-id
     :message-callbacks
     (ht ("current_conversation"
          (lambda (ws-message)
            (with-current-buffer buffer
              (aibo:go-to-conversation
               :conversation (ht-get ws-message "conversation")
               :buffer-open-style :nothing))))

         ("stream_assistant_message"
          (lambda (ws-message)
            (with-current-buffer buffer
              (widget-value-set
               aibo:b-streaming-assistant-message-widget
               (ht-get ws-message "message"))
              (set-buffer-modified-p nil))))

         ("event_completed" on-success)))))

(defun aibo:regenerate-current-conversation-last-assistant-message ()
  (interactive)
  (aibo:regenerate-last-assistant-message
   :conversation-id (ht-get aibo:b-conversation "id")))

(defun aibo:regenerate-last-assistant-message (&rest args)
  (interactive)
  (let* ((conversation-id (plist-get args :conversation-id))
         (on-success (plist-get args :on-success))
         (buffer (aibo:--get-conversation-buffer conversation-id)))
    (aibo:api-ws-regenerate-last-assistant-message
     :conversation-id conversation-id
     :message-callbacks
     (ht ("current_conversation"
          (lambda (ws-message)
            (with-current-buffer buffer
              (aibo:go-to-conversation
               :conversation (ht-get ws-message "conversation")
               :buffer-open-style :nothing))))
         ("stream_assistant_message"
          (lambda (ws-message)
            (with-current-buffer buffer
              (widget-value-set
               aibo:b-streaming-assistant-message-widget
               (ht-get ws-message "message"))
              (set-buffer-modified-p nil))))
         ("event_completed" on-success)))))

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
             (message-suffix (if (string= (ht-get message "status") "streaming") "█" "")))
        (concat
         (propertize
          (aibo::--get-message-header message)
          'read-only t
          'message message
          'font-lock-face `(:background ,message-header-color))
         (propertize
          (format "\n%s%s" (ht-get message "content_text") message-suffix)
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
             (ht-get message "id")
             message-widget)))

(defun aibo:--update-message-widget (message widget)
  (let* ((message-widget
          (ht-get aibo:b-message-widgets (ht-get message "id"))))
    (widget-value-set message-widget message)))

(defun aibo:--render-conversation (conversation)
  (let ((all-messages (ht-get conversation "all_messages"))
        (rendered-messages nil)
        (current-message-id (ht-get conversation "current_message_id")))

    (while current-message-id
      (let* ((message (ht-get all-messages current-message-id)))
        (if message
            (progn
              (push message rendered-messages)
              (setq current-message-id (ht-get message "parent_id")))
          (setq current-message-id nil))))

    (--each rendered-messages (aibo:--render-message-widget it))))


;; ---[ Conversation editing ]--------------------
(defun aibo:--message-at-point ()
  (interactive)
  (get-char-property (point) 'message))

(defun aibo:--widget-at-point ()
  (interactive)
  (let* ((message (aibo:--message-at-point)))
    (ht-get aibo:b-message-widgets (ht-get message "id"))))

(defun aibo:remove-message-at-point ()
  (interactive)
  (let* ((message (aibo:--message-at-point))
         (message-id (ht-get message "id")))
    (aibo:api-delete-message
     :conversation-id (ht-get aibo:b-conversation "id")
     :message-id message-id
     :delete-after nil
     :on-success (lambda (conversation)
                   (aibo:go-to-conversation :conversation conversation)))))

(defun aibo:remove-messages-after-point ()
  (interactive)
  (let* ((message (aibo:--message-at-point))
         (message-id (ht-get message "id")))
    (aibo:api-delete-message
     :conversation-id (ht-get aibo:b-conversation "id")
     :message-id message-id
     :delete-after t
     :on-success (lambda (conversation)
                   (aibo:go-to-conversation :conversation conversation)))))

;; TODO
(defun aibo:edit-message-at-point ()
  (interactive)
  (let* ((message (aibo:--message-at-point))
         (message-id (ht-get message "id")))))

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
            :history #'aibo:--create-conversation-history
            :action #'aibo:--create-conversation-from-template
            :caller #'aibo:--create-conversation-with-content))

(ivy-configure 'aibo:--create-conversation-with-content
  :initial-input "^")

(defun aibo:--create-conversation-from-template (template-option)
  (let* ((template-short-name (car (split-string template-option "\\s-+")))
         (template (aibo:get-conversation-template
                    :short-name template-short-name))
         (action-type (oref template :action-type))
         (buffer (current-buffer))
         (current-point (point))
         (content-input (read-string (format "%s: " (oref template :name))))
         (content (if (string= content-input "")
                      (buffer-substring (region-beginning) (region-end))
                    content-input))
         (message-inputs (aibo:get-conversation-template-message-inputs
                          :template template
                          :content content)))

    (aibo:api-create-conversation
     :message-inputs message-inputs
     :on-success
     (lambda (conversation)
       (cond
        ((eq action-type :new-conversation)
         (progn
           (aibo:go-to-conversation :conversation conversation)
           (with-current-buffer (current-buffer)
             (aibo:stream-assistant-message
              :conversation-id (ht-get conversation "id")
              :on-success #'aibo:generate-current-conversation-title))))

        ((eq action-type :buffer-insert)
         (aibo:api-ws-stream-assistant-message-chunks
          :conversation-id (ht-get conversation "id")
          :message-callbacks
          (ht ("stream_assistant_message_chunk"
               (lambda (ws-message)
                 (let* ((chunk (ht-get ws-message "chunk"))
                        (status (ht-get chunk "status"))
                        (text (cond
                               ((string= status "streaming") (ht-get chunk "text"))
                               ((string= status "error") (ht-get chunk "content")))))
                   (with-current-buffer buffer
                     (save-excursion
                       (goto-char current-point)
                       (insert text)
                       (setq current-point (+ current-point (length text))))))))
              ("event_completed"
               (lambda ()
                 (aibo:api-delete-conversation :id (ht-get conversation "id"))))))))))))

;; ---[ Search conversations ]--------------------
(defvar aibo:--conversation-message-search-history nil
  "History for 'aibo:conversation-message-search'")

(defun aibo:message-search ()
  (interactive)
  (let ((ivy-dynamic-exhibit-delay-ms 200))
    (ivy-read "[aibo] Search: "
              #'aibo:--ivy-conversation-message-search
              :dynamic-collection t
              :require-match t
              :history #'aibo:--conversation-message-search-history
              :action #'aibo:--conversation-message-search-action)))

(defun aibo:--ivy-conversation-message-search (query)
  (--map (format "%s: %s"
                 (ht-get it "conversation_id")
                 (replace-regexp-in-string "\n" "\\\\n" (ht-get it "content_text")))
         (aibo:api-conversation-message-search
          :query query
          :limit 100
          :sync t)))

(defun aibo:--conversation-message-search-action (search-result)
  (when (string-match "^\\(.*?\\):.*" search-result)
    (let ((conversation-id (match-string-no-properties 1 search-result)))
      (aibo:go-to-conversation-by-id
       :conversation-id conversation-id))))

(provide 'aibo-conversation)
