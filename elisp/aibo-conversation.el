;;; aibo-conversation.el --- Methods related to conversations and the conversation buffer view -*- lexical-binding: t -*-
(require 'dash)
(require 'ht)
(require 'widget)

(require 'aibo-api)
(require 'aibo-custom)
(require 'aibo-types)

;; ---[ Utils ]-----------------------------------
(setq aibo:--has-cached-packages nil)
(setq aibo:--cached-packages nil)

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
        (define-key map (kbd "TAB")         (lambda () (interactive) (insert "\t")))
        (define-key map (kbd "RET")         (lambda () (interactive) (insert "\n")))
        (define-key map (kbd "M-RET")       #'aibo:submit-user-message)
        map))

(defun aibo:--new-user-input-end-of-buffer-advice (&rest _)
  "Rather than going to the end of the buffer, go to the user input"
  (when (looking-at-p "^$")
    (backward-char)))

(defun aibo::--get-role-face (role location)
  (let* ((key (concat role "-" location)))
    (cond
     ((string= key "system-content")      'aibo:system-content-face)
     ((string= key "system-header")       'aibo:system-header-face)

     ((string= key "user-content")        'aibo:user-content-face)
     ((string= key "user-header")         'aibo:user-header-face)
     ((string= key "user-subheader")      'aibo:user-subheader-face)

     ((string= key "assistant-content")   'aibo:assistant-content-face)
     ((string= key "assistant-header")    'aibo:assistant-header-face)
     ((string= key "assistant-subheader") 'aibo:assistant-subheader-face)

     ((string= key "function-content")    'aibo:function-content-face)
     ((string= key "function-header")     'aibo:function-header-face)
     ((string= key "function-subheader")  'aibo:function-subheader-face)

     ((string= key "error-content")       'aibo:error-content-face)
     ((string= key "error-header")        'aibo:error-header-face))))

(defun aibo::--get-message-face (message location)
  (aibo::--get-role-face (ht-get message "role") location))

(defun aibo::--get-message-header (message)
  (let* ((role (ht-get message "role"))
         (message-header-face (aibo::--get-message-face message "header"))
         (message-subheader-face (aibo::--get-message-face message "subheader"))
         (message-content-face (aibo::--get-message-face message "content"))
         (header-text
          (cond
           ((string= role "system")    "System")
           ((string= role "user")      "User")
           ((string= role "assistant") "Assistant")
           ((string= role "function")  "Function")
           ((string= role "error")     "Error")))
         (subheader-text
          (cond
           ((string= role "assistant")
            (aibo::--get-message-source message))
           ((string= role "function")
            (replace-regexp-in-string "__" "." (aibo::--get-message-source message))))))
    (concat
     (propertize
      (format " %s " header-text)
      'read-only t
      'message message
      'font-lock-face message-header-face)
     (if subheader-text
         (propertize
          (format " %s " subheader-text)
          'read-only t
          'message message
          'font-lock-face message-subheader-face)
       ""))))

(defun aibo::--get-message-source (message)
  (let* ((source (ht-get message "source"))
         (source-kind (ht-get source "kind")))
    (cond
     ((string= source-kind "human")        (ht-get source "user"))
     ((string= source-kind "openai_model") (ht-get source "model"))
     ((string= source-kind "programmatic") (ht-get source "source")))))

(defun aibo:--refresh-cached-packages (&rest args)
  "Returns a list of package info: name + is-enabled"
  (let* ((on-success (plist-get args :on-success)))
    (aibo:api-get-packages
     :on-success (lambda (package-names)
                   (setq aibo:--has-cached-packages t)
                   (setq aibo:--cached-packages package-names)
                   (if on-success (funcall on-success package-names))))))

(defun aibo:refresh-packages ()
  "Returns a list of package info: name + is-enabled"
  (interactive)
  (aibo:--refresh-cached-packages
   :on-success (lambda (_)
                 (aibo:refresh-current-conversation))))

(defun aibo:get-packages (conversation)
  (if (null aibo:--has-cached-packages)
      (aibo:--refresh-cached-packages))
  (let* ((enabled-packages (ht-get conversation "enabled_package_names")))
    (--map
     (ht ("name" it)
         ("is-enabled" (member it enabled-packages)))
     (or aibo:--cached-packages enabled-packages))))

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
  (let* ((has-user-message-widget? (boundp 'aibo:b-new-user-message-widget))
         (current-user-message (if has-user-message-widget?
                                   (widget-value aibo:b-new-user-message-widget)
                                 ""))
         (set-not-modified (not (buffer-modified-p)))
         (original-point (if (> (buffer-size) 0) (point) nil))
         (original-window-start (window-start))
         (point-was-inside-user-widget?
          (and original-point
               has-user-message-widget?
               (>= original-point (widget-field-start aibo:b-new-user-message-widget))
               (<= original-point (widget-field-end aibo:b-new-user-message-widget))))
         (user-widget-point-shift
          (if point-was-inside-user-widget?
              (- original-point (widget-field-start aibo:b-new-user-message-widget))
            0)))

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

    ;; Render title
    (aibo:--on-conversation-title-change conversation)

    ;; Render header
    (aibo:--render-conversation-header conversation)

    ;; Render conversation
    (aibo:--render-conversation conversation)

    ;; Render streamed assistant completion
    (setq-local aibo:b-streaming-assistant-message-widget
                (widget-create 'chat-message :value nil))

    ;; Render User input
    (widget-insert (propertize
                    " User "
                    'font-lock-face (aibo::--get-role-face "user" "header")))
    (widget-insert "\n")
    (setq-local aibo:b-new-user-message-widget
                (widget-create 'text
                               :value current-user-message
                               :format "%v"
                               :value-face 'aibo:user-message-input-face
                               :keymap aibo:--new-user-message-keymap))

    ;; Clean up buffer state
    (if set-not-modified
        (set-buffer-modified-p nil))

    (widget-setup)

    (if (or point-was-inside-user-widget? (not original-point))
        (progn
          (goto-char (+
                      (widget-field-start aibo:b-new-user-message-widget)
                      user-widget-point-shift))
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
   :on-success #'aibo:--on-conversation-title-change))

(defun aibo:set-conversation-title (conversation)
  (interactive)
  (let* ((id (ht-get conversation "id"))
         (title (read-string "Title: ")))
    (aibo:api-set-conversation-title
     :id id
     :title title
     :on-success #'aibo:--on-conversation-title-change)))

(defun aibo:--on-conversation-title-change (conversation)
  (setq header-line-format
          (format "[%s] %s"
                  aibo:model
                  (ht-get conversation "title")))
  (save-window-excursion (aibo:homepage)))

(defun aibo:--render-conversation-header (conversation)
  (let* ((id (ht-get conversation "id"))
         (packages (aibo:get-packages conversation)))
    (widget-insert (propertize
                    " Conversation "
                    'font-lock-face 'aibo:conversation-header-face))
    (widget-insert (propertize
                    (format " %s " id)
                    'font-lock-face 'aibo:conversation-subheader-face))
    (widget-insert "\n")
    (if packages
        (widget-insert
         (propertize
          "Packages\n"
          'font-lock-face 'aibo:conversation-header-content-face)))
    (--each packages
      (widget-insert (propertize
                      (format
                       "[%s] %s\n"
                       (if (ht-get it "is-enabled") "x" " ")
                       (ht-get it "name"))
                      'font-lock-face 'aibo:conversation-header-content-face)))))

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
  :value-create 'aibo:--maybe-render-chat-message-widget)

(defun aibo:--maybe-render-chat-message-widget (widget)
  (aibo:--render-chat-message-widget (widget-get widget :value)))

(defun aibo:--render-chat-message-widget (message)
  (widget-insert "\n")
  (if message
      (let* ((message-content-face (aibo::--get-message-face message "content"))
             (message-suffix (if (string= (ht-get message "status") "streaming") "█\n" ""))
             (message-content (ht-get message "content_text")))
        (widget-insert
         (concat
          (aibo::--get-message-header message)
          (propertize
           "\n"
           'read-only t
           'message message
           'font-lock-face message-content-face)))

        (aibo:--render-message-content
         :message message
         :text message-content
         :font-lock-face message-content-face)

        (widget-insert
         (concat
          (propertize
           message-suffix
           'read-only t
           'message message
           'font-lock-face message-content-face)
          (propertize
           "\n"
           'read-only t
           'message message))))))

(defun aibo:--render-message-content (&rest args)
  (interactive)
  (let* (;; Args
         (message (plist-get args :message))
         (text (plist-get args :text))
         (font-lock-face (plist-get args :font-lock-face))
         ;; Vars
         (image-ranges (s-matched-positions-all "\\[Image:\\([^]]\\)+\\]" text)))
    (if (null image-ranges)
        ;; No images, so return the whole text
        (widget-insert
         (propertize
          text
          'read-only t
          'message message
          'font-lock-face font-lock-face))
      ;; We found images, render each image separately
      (--each-indexed
          image-ranges
        (let* ((is-first (= it-index 0))
               (next-it (nth (+ it-index 1) image-ranges))
               (next-start (or (-first-item next-it) (length text)))
               ;; Image text
               (image-text-start (car it))
               (image-text-end (cdr it))
               (image-text (substring text image-text-start image-text-end))
               ;; Image ID
               (image-id-start (+ image-text-start 7))
               (image-id-end (- image-text-end 1))
               (image-id (substring text image-id-start image-id-end)))
          (if (and is-first (< 0 image-text-start))
              (widget-insert
               (propertize
                (substring text 0 image-text-start)
                'read-only t
                'message message
                'font-lock-face font-lock-face)))

          (widget-create
           'link
           :notify (lambda (&rest ignore)
                     (shell-command
                      (format "open http://localhost:%s/images/%s"
                              aibo:server-port
                              image-id)))
           :button-prefix ""
           :button-suffix ""
           :tag (propertize
                 image-text
                 'read-only t
                 'message message
                 'font-lock-face font-lock-face))

          (if (< image-text-end next-start)
              (widget-insert
               (propertize
                (substring text image-text-end next-start)
                'read-only t
                'message message
                'font-lock-face font-lock-face)))
          )))))

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
   (aibo:conversation-templates)))

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
              :conversation-id (ht-get conversation "id")))))

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
