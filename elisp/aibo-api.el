;;; aibo-api.el --- Aibo HTTP and event client -*- lexical-binding: t -*-

(require 'json)
(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'aibo-custom)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)
(declare-function websocket-close "websocket")
(declare-function websocket-frame-payload "websocket")
(declare-function websocket-open "websocket")
(declare-function websocket-openp "websocket")

(defvar aibo:api-event-functions nil
  "Functions called with each decoded server event.")

(defvar aibo:api--websocket nil)
(defvar aibo:api--events-enabled nil)
(defvar aibo:api--reconnect-timer nil)
(defvar aibo:api--connection-generation 0)
(defvar aibo:api-connection-functions nil
  "Functions called with `connected' or `reconnecting'.")

(defun aibo:api--decode-buffer ()
  (goto-char (or url-http-end-of-headers (point-min)))
  (unless (eobp)
    (json-parse-buffer :object-type 'hash-table
                       :array-type 'list
                       :null-object nil
                       :false-object nil)))

(defun aibo:api--error-detail ()
  (when-let* ((payload (ignore-errors (aibo:api--decode-buffer)))
              (detail (and (hash-table-p payload) (gethash "detail" payload))))
    (if (stringp detail) detail (json-serialize detail))))

(defun aibo:api--handle-response
    (response-buffer status method path on-success on-error)
  (unwind-protect
      (with-current-buffer response-buffer
        (let ((network-error (plist-get status :error))
              (http-error (and url-http-response-status
                               (>= url-http-response-status 400))))
          (if (or network-error http-error)
              (if on-error
                  (funcall on-error status)
                (message "Aibo request failed: %s %s%s"
                         method path
                         (if http-error
                             (format " (HTTP %s%s)"
                                     url-http-response-status
                                     (if-let ((detail (aibo:api--error-detail)))
                                         (concat ": " detail)
                                       ""))
                           (format " (%s)" network-error))))
            (when on-success
              (funcall on-success (aibo:api--decode-buffer))))))
    (when (buffer-live-p response-buffer) (kill-buffer response-buffer))))

(defun aibo:api--request (method path &optional body on-success on-error)
  (let ((url-request-method method)
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data (and body (encode-coding-string
                                     (json-serialize body) 'utf-8))))
    (url-retrieve
     (concat aibo:server-url path)
     (lambda (status)
       (aibo:api--handle-response
        (current-buffer) status method path on-success on-error))
     nil t t)))

(defun aibo:api-get-chats (on-success &optional query limit offset on-error)
  (let ((parameters (delq nil
                          (list (and query
                                     (concat "query=" (url-hexify-string query)))
                                (and limit (format "limit=%d" limit))
                                (and offset (format "offset=%d" offset))))))
    (aibo:api--request
     "GET" (concat "/api/chats"
                   (and parameters (concat "?" (string-join parameters "&"))))
     nil on-success on-error)))

(defun aibo:api-get-project-chats (project-id on-success &optional on-error)
  "Get a home-page group independently of the recent-chat cache."
  (aibo:api--request
   "GET" (concat "/api/chats?limit=10&"
                 (if project-id (concat "project_id=" (url-hexify-string project-id))
                   "unassigned=true"))
   nil on-success on-error))

(defun aibo:api-get-chats-sync (&optional query limit)
  (let* ((parameters (delq nil
                           (list (and query
                                      (concat "query=" (url-hexify-string query)))
                                 (and limit (format "limit=%d" limit)))))
         (url (concat aibo:server-url "/api/chats"
                      (and parameters
                           (concat "?" (string-join parameters "&")))))
         (buffer (url-retrieve-synchronously url t t 2)))
    (unless buffer (user-error "Aibo server is unavailable"))
    (unwind-protect
        (with-current-buffer buffer
          (when (and url-http-response-status (>= url-http-response-status 400))
            (user-error "Aibo search failed with HTTP %s"
                        url-http-response-status))
          (or (aibo:api--decode-buffer) nil))
      (kill-buffer buffer))))

(defun aibo:api-get-chat (chat-id on-success &optional on-error)
  (aibo:api--request "GET" (format "/api/chats/%s/history" chat-id) nil on-success on-error))

(defun aibo:api-create-chat (kind on-success &optional location-id project-id on-error)
  (let ((body `((kind . ,kind))))
    (when location-id
      (setq body (append body `((location_id . ,location-id)))))
    (when project-id
      (setq body (append body `((project_id . ,project-id)))))
    (aibo:api--request "POST" "/api/chats" body on-success on-error)))

(defun aibo:api-submit (chat-id text attachments on-success &optional on-error)
  (aibo:api--request
   "POST" (format "/api/chats/%s/submit?compact=true" chat-id)
   `((text . ,text) (attachments . ,(vconcat attachments)))
   on-success on-error))

(defun aibo:api-set-goal (chat-id enabled on-success)
  "Set CHAT-ID's goal to its last user message, or disable it."
  (aibo:api--request
   "PUT" (format "/api/chats/%s/goal" chat-id)
   `((enabled . ,(if enabled t :false))) on-success))

(defun aibo:api-get-projects (on-success &optional archived on-error)
  (aibo:api--request
   "GET" (format "/api/projects?archived=%s" (if archived "true" "false"))
   nil on-success on-error))

(defun aibo:api-get-locations (on-success &optional on-error)
  (aibo:api--request "GET" "/api/locations" nil on-success on-error))

(defun aibo:api-get-sidebar (on-success &optional on-error)
  (aibo:api--request "GET" "/api/notifications" nil on-success on-error))

(defun aibo:api-read-notification (notification-id on-success)
  (aibo:api--request
   "POST" (format "/api/notifications/%s/read" notification-id)
   nil on-success))

(defun aibo:api-read-chat-notifications (chat-id on-success)
  (aibo:api--request "POST" (format "/api/chats/%s/notifications/read" chat-id)
                     nil on-success))

(defun aibo:api-update-chat (chat-id updates on-success)
  (aibo:api--request
   "PATCH" (format "/api/chats/%s" chat-id) updates on-success))

(defun aibo:api--event-url ()
  (concat (replace-regexp-in-string
           "\\`http" "ws" aibo:server-url)
          "/api/events?compact=true"))

(defun aibo:api--reconnect ()
  (when (and aibo:api--events-enabled (not (timerp aibo:api--reconnect-timer)))
    (run-hook-with-args 'aibo:api-connection-functions 'reconnecting)
    (setq aibo:api--reconnect-timer
          (run-with-timer 2 nil #'aibo:api-connect-events))))

(defun aibo:api-connect-events ()
  (interactive)
  (setq aibo:api--events-enabled t)
  (unless (require 'websocket nil t)
    (user-error "The websocket.el package is required for live Aibo events"))
  (when (timerp aibo:api--reconnect-timer)
    (cancel-timer aibo:api--reconnect-timer)
    (setq aibo:api--reconnect-timer nil))
  (unless aibo:api--websocket
    (let ((generation (cl-incf aibo:api--connection-generation)))
      (condition-case nil
          (setq aibo:api--websocket
                (websocket-open
                 (aibo:api--event-url)
                 :on-open
                 (lambda (_websocket)
                   (when (= generation aibo:api--connection-generation)
                     (run-hook-with-args 'aibo:api-connection-functions 'connected)))
                 :on-message
                 (lambda (_websocket frame)
                   (when (= generation aibo:api--connection-generation)
                     (let ((event (json-parse-string
                                   (websocket-frame-payload frame)
                                   :object-type 'hash-table
                                   :array-type 'list)))
                       (run-hook-with-args 'aibo:api-event-functions event))))
                 :on-close
                 (lambda (_websocket)
                   (when (= generation aibo:api--connection-generation)
                     (setq aibo:api--websocket nil)
                     (aibo:api--reconnect)))))
        (error (setq aibo:api--websocket nil)
               (aibo:api--reconnect))))))

(defun aibo:api-disconnect-events ()
  (setq aibo:api--events-enabled nil)
  (cl-incf aibo:api--connection-generation)
  (when (timerp aibo:api--reconnect-timer)
    (cancel-timer aibo:api--reconnect-timer)
    (setq aibo:api--reconnect-timer nil))
  (when aibo:api--websocket
    (websocket-close aibo:api--websocket))
  (setq aibo:api--websocket nil))

(provide 'aibo-api)
