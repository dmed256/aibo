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
  (if (not (aibo:api-is-healthy))
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
                            "--port" (number-to-string aibo:server-port))))))))

(setq aibo:--on-healthy-server-max-checks 5)
(setq aibo:--on-healthy-server-checks nil)

(defun aibo:--on-healthy-server (&rest args)
  (if (not aibo:--on-healthy-server-checks)
      (setq aibo:--on-healthy-server-checks 0)
    (setq aibo:--on-healthy-server-checks (+ aibo:--on-healthy-server-checks 1)))

  (let* ((on-success (plist-get args :on-success)))
    (if (aibo:api-is-healthy)
        (progn
          (setq aibo:--on-healthy-server-checks nil)
          (funcall on-success))
      (if (< aibo:--on-healthy-server-checks aibo:--on-healthy-server-max-checks)
          (run-with-timer 1 nil #'aibo:--on-healthy-server
                          :on-success on-success)))))


;; ---[ Request Wrappers ]------------------------
(defun aibo:--request-json-parser ()
  (let ((json-object-type 'hash-table)
        (json-array-type 'list))
    (json-read)))

(defun aibo:--api-request (&rest args)
  (let* ((path (plist-get args :path))
         (type (plist-get args :type))
         (data (plist-get args :data))
         (sync (plist-get args :sync))
         (timeout (plist-get args :timeout))
         (response-transform (plist-get args :response-transform))
         (on-success (plist-get args :on-success))
         (response (request (format "http://localhost:%s%s" aibo:server-port path)
                     :type type
                     :headers '(("Content-Type" . "application/json"))
                     :parser #'aibo:--request-json-parser
                     :encoding 'utf-8
                     :sync sync
                     :timeout (or timeout 1)
                     :data (if data (json-encode data) nil)
                     :success (lambda (&rest args)
                                (if on-success
                                    (let* ((data (plist-get args :data))
                                           (on-success-args (if response-transform
                                                                (funcall response-transform data)
                                                              data)))
                                      (funcall on-success on-success-args)))))))
    (if sync
        (let* ((data (request-response-data response)))
          (if response-transform
              (funcall response-transform data)
            data)))))


(defun aibo:--api-get (&rest args)
  (apply 'aibo:--api-request :type "GET" args))

(defun aibo:--api-post (&rest args)
  (apply 'aibo:--api-request :type "POST" args))

(defun aibo:--api-patch (&rest args)
  (apply 'aibo:--api-request :type "PATCH" args))

(defun aibo:--api-delete (&rest args)
  (apply 'aibo:--api-request :type "DELETE" args))


;; ---[ API Methods ]-----------------------------
(defun aibo:api-is-healthy ()
  (string= "OK" (aibo:--api-get
                 :path "/status"
                 :bypass-server-check t
                 :sync t
                 :timeout 0.1)))

(defun aibo:api-get-conversations (&rest args)
  (let* ((limit (plist-get args :limit))
         (on-success (plist-get args :on-success)))
    (aibo:--api-post
     :path "/chat/conversations/search"
     :data (ht ("limit" limit))
     :response-transform (lambda (response) (ht-get response "conversations"))
     :on-success on-success)))

(defun aibo:api-get-conversation (&rest args)
  (let* ((conversation-id (plist-get args :conversation-id))
         (on-success (plist-get args :on-success)))
    (aibo:--api-get
     :path (format "/chat/conversations/%s" conversation-id)
     :response-transform (lambda (response) (ht-get response "conversation"))
     :on-success on-success)))

(defun aibo:api-create-conversation (&rest args)
  (let* ((model (plist-get args :model))
         (message-inputs (plist-get args :message-inputs))
         (on-success (plist-get args :on-success)))
    (aibo:--api-post
     :path "/chat/conversations"
     :data (ht ("messages"              message-inputs)
               ("model"                 (or model aibo:model))
               ("enabled_package_names" aibo:enabled-package-names)
               ("temperature"           aibo:temperature)
               ("shorthands"            (aibo:--conversation-shorthands)))
     :response-transform (lambda (response) (ht-get response "conversation"))
     :on-success on-success)))

(defun aibo:api-delete-conversation (&rest args)
  (let* ((conversation-id (plist-get args :conversation-id))
         (on-success (plist-get args :on-success)))
    (aibo:--api-delete
     :path (format "/chat/conversations/%s" conversation-id)
     :on-success on-success)))

(defun aibo:api-submit-user-message (&rest args)
  (let* ((conversation-id (plist-get args :conversation-id))
         (text (plist-get args :text))
         (on-success (plist-get args :on-success)))
    (aibo:--api-post
     :path (format "/chat/conversations/%s/submit-user-message" conversation-id)
     :data (ht ("text" text))
     :response-transform (lambda (response) (ht-get response "conversation"))
     :on-success on-success)))

(defun aibo:api-edit-message (&rest args)
  (let* ((conversation-id (plist-get args :conversation-id))
         (message-id (plist-get args :message-id))
         (text (plist-get args :text))
         (on-success (plist-get args :on-success)))
    (aibo:--api-put
     :path (format "/chat/conversations/%s/messages/%s" conversation-id message-id)
     :data (ht ("text" text))
     :response-transform (lambda (response) (ht-get response "conversation"))
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
     :response-transform (lambda (response) (ht-get response "conversation"))
     :on-success on-success)))

(defun aibo:api-set-conversation-title (&rest args)
  (let* ((conversation-id (plist-get args :conversation-id))
         (title (plist-get args :title))
         (on-success (plist-get args :on-success)))
    (aibo:--api-patch
     :path (format "/chat/conversations/%s" conversation-id)
     :data (ht (:title title))
     :response-transform (lambda (response) (ht-get response "conversation"))
     :on-success on-success)))

(defun aibo:api-conversation-message-search (&rest args)
  (interactive)
  (let* ((query (plist-get args :query))
         (limit (plist-get args :limit))
         (sync (plist-get args :sync)))
    (aibo:--api-post
     :path "/chat/conversations/message-search"
     :data (ht (:query query)
               (:limit limit))
     :sync sync
     :response-transform (lambda (response) (ht-get response "search_results")))))

(defun aibo:api-generate-conversation-title (&rest args)
  (let* ((conversation-id (plist-get args :conversation-id))
         (on-success (plist-get args :on-success)))
    (aibo:--api-post
     :path (format "/chat/conversations/%s/generate-title" conversation-id)
     :data (ht ("model" aibo:model))
     :response-transform (lambda (response) (ht-get response "conversation"))
     :on-success on-success)))

(defun aibo:api-get-packages (&rest args)
  (let* ((on-success (plist-get args :on-success)))
    (aibo:--api-get
     :path "/chat/packages"
     :response-transform
     (lambda (response)
       (--sort #'string< (ht-get response "packages")))
     :on-success on-success)))

(defun aibo:api-set-package-enabled (&rest args)
  (let* ((conversation-id (plist-get args :conversation-id))
         (package-name (plist-get args :package-name))
         (is-enabled (plist-get args :is-enabled))
         (on-success (plist-get args :on-success)))
    (aibo:--api-patch
     :path (format "/chat/conversations/%s/packages" conversation-id)
     :data (ht ("package_enabled_updates" (ht (package-name is-enabled))))
     :response-transform (lambda (response) (ht-get response "conversation"))
     :on-success on-success)))


;; ---[ Websocket ]-------------------------------
(defun aibo:--api-ws-send (&rest args)
  (let* ((event (plist-get args :event))
         (message-callbacks (plist-get args :message-callbacks))
         (event-id (uuidgen-4)))
    (ht-set! event "id" event-id)
    (let ((websocket
           (websocket-open
            (format "ws://localhost:%s/ws" aibo:server-port)
            :on-message
            (lambda (_websocket frame)
              (let* ((event (json-parse-string (websocket-frame-payload frame)
                                               :object-type 'hash-table
                                               :array-type 'list))
                     (event-kind (ht-get event "kind"))
                     (callback (ht-get message-callbacks event-kind)))
                (when callback
                  (funcall callback event))
                (when (string= event-kind "event_completed")
                  (websocket-close _websocket))))
            :on-close
            (lambda (_websocket)))))
      (websocket-send-text websocket (json-encode event))
      websocket)))

(defun aibo:api-ws-stream-assistant-message (&rest args)
  (let* ((conversation-id (plist-get args :conversation-id))
         (model (plist-get args :model))
         (message-callbacks (plist-get args :message-callbacks)))
    (aibo:--api-ws-send
     :event (ht ("kind"            "stream_assistant_message")
                ("conversation_id" conversation-id)
                ("model"           (or model aibo:model))
                ("temperature"     aibo:temperature))
     :message-callbacks message-callbacks)))

(defun aibo:api-ws-regenerate-last-assistant-message (&rest args)
  (let* ((conversation-id (plist-get args :conversation-id))
         (message-callbacks (plist-get args :message-callbacks)))
    (aibo:--api-ws-send
     :event (ht ("kind"            "regenerate_last_assistant_message")
                ("conversation_id" conversation-id)
                ("model"           aibo:model))
     :message-callbacks message-callbacks)))

(defun aibo:api-ws-stream-assistant-message-chunks (&rest args)
  (let* ((conversation-id (plist-get args :conversation-id))
         (message-callbacks (plist-get args :message-callbacks)))
    (aibo:--api-ws-send
     :event (ht ("kind"            "stream_assistant_message_chunks")
                ("conversation_id" conversation-id)
                ("model"           aibo:model)
                ("temperature"     aibo:temperature))
     :message-callbacks message-callbacks)))


;; ---[ Utils ]-----------------------------------
(defun aibo:--conversation-shorthands ()
  (let* ((header (make-string 20 ?-))
         (region-string (if (mark)
                            (buffer-substring (min (mark) (point)) (max (mark) (point)))
                          ""))
         (region-value (if (> (length region-string) 0)
                           (format "\n\n%s\n%s\n%s\n\n" header region-string header)
                         " "))
         (buffer-value (format "\n\n%s\n%s\n%s\n\n" header (buffer-string) header)))
    (ht ("r" region-value)
        ("b" buffer-value))))

(provide 'aibo-api)
