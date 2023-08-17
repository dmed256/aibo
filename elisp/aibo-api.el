;;; aibo-api.el --- API calls to the Python server -*- lexical-binding: t -*-
(require 'aibo-custom)
(require 'aibo-types)
(require 'aibo-utils)

(require 'dash)
(require 'eieio)
(require 'ht)
(require 'json)
(require 'request)
(require 'uuidgen)
(require 'websocket)

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
                        aibo:server-python
                        "-m" "aibo.cli.start"
                        "--port" (number-to-string aibo:server-port))))
     :on-load
     (lambda (buffer)
       (funcall on-success)))))


;; ---[ Request Wrappers ]------------------------
(defun aibo:--api-request (&rest args)
  (let* ((path (plist-get args :path))
         (type (plist-get args :type))
         (data (plist-get args :data))
         (response-transform (plist-get args :response-transform))
         (on-success (plist-get args :on-success))
         (bypass-server-check (plist-get args :bypass-server-check))
         (do-request
          (lambda ()
            (request (format "http://localhost:%s%s" aibo:server-port path)
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


;; ---[ API Methods ]-----------------------------
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

(defun aibo:api-edit-message (&rest args)
  (let* ((conversation-id (plist-get args :conversation-id))
         (message-id (plist-get args :message-id))
         (text (plist-get args :text))
         (on-success (plist-get args :on-success)))
    (aibo:--api-put
     :path (format "/chat/conversations/%s/messages/%s" conversation-id message-id)
     :data `(("text" . ,text))
     :response-transform (lambda (response)
                           (let* ((api-conversation (cdr (assoc 'conversation response))))
                             (Conversation-from-api api-conversation)))
     :on-success on-success)))

(defun aibo:api-delete-message (&rest args)
  (let* ((conversation-id (plist-get args :conversation-id))
         (message-id (plist-get args :message-id))
         (delete-after (plist-get args :delete-after))
         (on-success (plist-get args :on-success)))
    (aibo:--api-delete
     :path (format "/chat/conversations/%s/messages/%s?delete-after=%s"
                   conversation-id
                   message-id
                   (if delete-after "true" "false"))
     :response-transform (lambda (response)
                           (let* ((api-conversation (cdr (assoc 'conversation response))))
                             (Conversation-from-api api-conversation)))
     :on-success on-success)))

(defun aibo:api-regenerate-assistant-message (&rest args)
  (let* ((conversation-id (plist-get args :conversation-id))
         (on-success (plist-get args :on-success)))
    (aibo:--api-post
     :path (format "/chat/conversations/%s/regenerate-assistant-message" conversation-id)
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


;; ---[ Websocket ]-------------------------------
(setq aibo:--websocket-callbacks (ht-create))
(setq aibo:--websocket nil)

(defun aibo:websocket ()
  (interactive)
  (or aibo:--websocket (aibo:--connect-websocket)))

(defun aibo:--connect-websocket ()
  (interactive)
  (setq aibo:--websocket
        (websocket-open
         (format "ws://localhost:%s/ws" aibo:server-port)
         :on-message
         (lambda (websocket frame)
           (let* ((event (json-parse-string (websocket-frame-payload frame)))
                  (event-id (ht-get event "id"))
                  (event-kind (ht-get event "kind"))
                  (is-completed (string= event-kind "event_completed"))
                  (callback (ht-get aibo:--websocket-callbacks event-id)))
             (if callback
                 (funcall callback event is-completed))
             (if is-completed
                 (ht-remove! aibo:--websocket-callbacks event-id))))
         :on-close
         (lambda (websocket)
           (setq aibo:--websocket nil)
           (setq aibo:--websocket-callbacks (ht-create))))))

(defun aibo:--api-ws-send (&rest args)
  (let* ((partial-event (plist-get args :event))
         (response-transform (plist-get args :response-transform))
         (on-message (plist-get args :on-message))
         (on-success (plist-get args :on-success))
         (event-id (uuidgen-4))
         (event (add-to-list 'partial-event `("id" . ,event-id))))
    (ht-set! aibo:--websocket-callbacks
             event-id
             (lambda (ht-event is-completed)
               (let* ((event (ht->alist ht-event)))
                 (if is-completed
                     (funcall on-success)
                     (funcall on-message (funcall response-transform event))))))
    (websocket-send-text (aibo:websocket) (json-encode event))))

(defun aibo:api-ws-submit-user-message (&rest args)
  (let* ((conversation-id (plist-get args :conversation-id))
         (text (plist-get args :text))
         (on-message (plist-get args :on-message))
         (on-success (plist-get args :on-success)))
    (aibo:--api-ws-send
     :event `(("conversation_id" . ,conversation-id)
              ("text" . ,text))
     :response-transform (lambda (api-message)
                           (Message-from-api api-message))
     :on-message on-message
     :on-success on-success)))

(provide 'aibo-api)
