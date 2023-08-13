;;; aibo-api.el --- API calls to the Python server -*- lexical-binding: t -*-
(require 'aibo-custom)
(require 'aibo-types)
(require 'aibo-utils)

(require 'dash)
(require 'eieio)
(require 'json)
(require 'request)

(defun aibo:start-server (&rest args)
  (interactive)
  (let* ((on-success (plist-get args :on-success)))
    (aibo:--get-or-create-buffer
     :name "*Aibo server*"
     :on-create
     (lambda (buffer)
       (with-current-buffer buffer
         (add-hook 'after-change-functions 'aibo:--ansi-buffer nil t)
         (start-process "aibo-server" buffer
                        aibo:python
                        "-m" "aibo.cli.start"
                        "--port" (number-to-string aibo:python-port))))
     :on-load
     (lambda (buffer)
       (funcall on-success)))))

(defun aibo:--api-request (&rest args)
  (let* ((path (plist-get args :path))
         (type (plist-get args :type))
         (data (plist-get args :data))
         (response-transform (plist-get args :response-transform))
         (on-success (plist-get args :on-success))
         (bypass-server-check (plist-get args :bypass-server-check))
         (do-request
          (lambda ()
            (request (format "http://localhost:%s%s" aibo:python-port path)
              :type type
              :headers '(("Content-Type" . "application/json"))
              :parser 'json-read
              :encoding 'utf-8
              :data (if data (json-encode data) nil)
              :success (lambda (&rest args)
                         (if on-success
                             (let* ((data (plist-get args :data))
                                    (on-success-args (if response-transform
                                                         (funcall response-transform data)
                                                       data)))
                               (funcall on-success on-success-args))))))))
    (if bypass-server-check
        (funcall do-request)
        (aibo:start-server :on-success do-request))))

(defun aibo:--api-get (&rest args)
  (apply 'aibo:--api-request :type "GET" args))

(defun aibo:--api-post (&rest args)
  (apply 'aibo:--api-request :type "POST" args))

(defun aibo:--api-patch (&rest args)
  (apply 'aibo:--api-request :type "PATCH" args))

(defun aibo:--api-delete (&rest args)
  (apply 'aibo:--api-request :type "DELETE" args))

(defun aibo:api-get-conversations (&rest args)
  (let* ((on-success (plist-get args :on-success)))
    (aibo:--api-post
     :path "/chat/conversations/search"
     :response-transform (lambda (response)
                           (let* ((api-conversations (cdr (assoc 'conversations response))))
                             (--map (ConversationSummary-from-api it) api-conversations)))
     :on-success on-success)))

(defun aibo:api-get-conversation (&rest args)
  (let* ((id (plist-get args :id))
         (on-success (plist-get args :on-success)))
    (aibo:--api-get
     :path (format "/chat/conversations/%s" id)
     :response-transform (lambda (response)
                           (let* ((api-conversation (cdr (assoc 'conversation response))))
                             (Conversation-from-api api-conversation)))
     :on-success on-success)))

(defun aibo:api-create-conversation (&rest args)
  (let* ((message-inputs (plist-get args :message-inputs))
         (on-success (plist-get args :on-success)))
    (aibo:--api-post
     :path "/chat/conversations"
     :data `(("messages" . ,message-inputs)
             ("model_name" . ,aibo:model-name))
     :response-transform (lambda (response)
                           (let* ((api-conversation (cdr (assoc 'conversation response))))
                             (Conversation-from-api api-conversation)))
     :on-success on-success)))

(defun aibo:api-delete-conversation (&rest args)
  (let* ((id (plist-get args :id))
         (on-success (plist-get args :on-success)))
    (aibo:--api-delete
     :path (format "/chat/conversations/%s" id)
     :on-success on-success)))

(defun aibo:api-submit-user-message (&rest args)
  (let* ((conversation-id (plist-get args :conversation-id))
         (text (plist-get args :text))
         (on-success (plist-get args :on-success)))
    (aibo:--api-post
     :path (format "/chat/conversations/%s/submit-user-message" conversation-id)
     :data `(("text" . ,text))
     :response-transform (lambda (response)
                           (let* ((api-conversation (cdr (assoc 'conversation response))))
                             (Conversation-from-api api-conversation)))
     :on-success on-success)))

(defun aibo:api-set-conversation-title (&rest args)
  (let* ((id (plist-get args :id))
         (title (plist-get args :title))
         (on-success (plist-get args :on-success)))
    (aibo:--api-patch
     :path (format "/chat/conversations/%s" id)
     :data `((:title . ,title))
     :response-transform (lambda (response)
                           (let* ((api-conversation (cdr (assoc 'conversation response))))
                             (Conversation-from-api api-conversation)))
     :on-success on-success)))

(defun aibo:api-generate-conversation-title (&rest args)
  (let* ((id (plist-get args :id))
         (on-success (plist-get args :on-success)))
    (aibo:--api-post
     :path (format "/chat/conversations/%s/generate-title" id)
     :response-transform (lambda (response)
                           (let* ((api-conversation (cdr (assoc 'conversation response))))
                             (Conversation-from-api api-conversation)))
     :on-success on-success)))

(provide 'aibo-api)
