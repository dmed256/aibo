;;; aibo-types.el --- Most of the eieio class definitions -*- lexical-binding: t -*-
(require 'dash)
(require 'eieio)
(require 'ts)

;; ---[ API Classes ]-----------------------------
(defclass ConversationSummary ()
  ((id
    :documentation "The primary ID of the conversation"
    :initarg :id
    :type string)
   (title
    :documentation "Human-readable title"
    :initarg :title
    :type string)
   (created-at
    :documentation "When the conversation was created"
    :initarg :created-at
    :type ts)))

(defclass Conversation (ConversationSummary)
  ((root-message-id
    :documentation "The root message ID"
    :initarg :root-message-id
    :type string)
   (current-message-id
    :documentation "The last message leaf ID created or edited in the conversation"
    :initarg :current-message-id
    :type string)
   (all-messages
    :documentation "The messages in the conversation"
    :initarg :all-messages)))

(defun ConversationSummary-from-api (api-conversation)
  (ConversationSummary
   :id (cdr (assoc 'id api-conversation))
   :title (cdr (assoc 'title api-conversation))
   :created-at (ts-parse (cdr (assoc 'created_at api-conversation)))))

(defun Conversation-from-api (api-conversation)
  (let ((summary (ConversationSummary-from-api api-conversation)))
    (Conversation
     :id (oref summary :id)
     :title (oref summary :title)
     :created-at (oref summary :created-at)
     :root-message-id (cdr (assoc 'root_message_id api-conversation))
     :current-message-id (cdr (assoc 'current_message_id api-conversation))
     :all-messages (--map
                    (cons (car it) (Message-from-api it))
                    (cdr (assoc 'all_messages api-conversation))))))

(defclass Message ()
  ((id
    :documentation "The primary ID of the message"
    :initarg :id
    :type string)
   (status
    :documentation "The status, such as 'streaming' or 'completed'"
    :initarg :status
    :type string)
   (parent-id
    :documentation "If set, the parent message ID"
    :initarg :parent-id
    :initform nil)
   (source
    :documentation "Message source"
    :initarg :source)
   (role
    :documentation "Message role"
    :initarg :role
    :type string)
   (content-text
    :documentation "Message content"
    :initarg :content-text
    :type string)))

(defun Message-from-api (api-message)
  (Message
   :id (cdr (assoc 'id api-message))
   :status (cdr (assoc 'status api-message))
   :parent-id (cdr (assoc 'parent_id api-message))
   :source (cdr (assoc 'source api-message))
   :role (cdr (assoc 'role api-message))
   :content-text (cdr (assoc 'content_text api-message))))

(defclass HumanSource ()
  ((kind
    :initarg :kind
    :initform "human")
   (user
    :documentation "User information"
    :initarg :user
    :type string)))

(defclass OpenAIModelSource ()
  ((kind
    :initarg :kind
    :initform "openai_model")
   (model
    :documentation "Which model generated the attached message"
    :initarg :model
    :type string)
   (temperature
    :documentation "Temperature used to generate the message"
    :initarg :temperature
    :type float)
   (max-tokens
    :documentation "Tokens used to generate the message"
    :initarg :max-tokens)))

(defclass ProgrammaticSource ()
  ((kind
    :initarg :kind
    :initform "programmatic")
   (source
    :documentation "The tool that generated the attached message"
    :initarg :source
    :type string)))

;; ---[ ConversationSection ]---------------------
(defclass ConversationSection ()
  ((name
    :documentation "Section name"
    :initarg :name
    :type string)
   (priority
    :documentation "The higher priority, the higher the sectionc ontent will show up"
    :initarg :priority
    :type integer)
   (conversations
    :documentation "Conversations in the section"
    :initarg :conversations
    :initform (list)
    :type list)))

(cl-defmethod ConversationSection-from ((conversation ConversationSummary))
  (let* ((now (ts-now))
         (today-start (ts-apply :hour 0 :minute 0 :second 0 now))
         (yesterday-start (ts-adjust 'day -1 today-start))
         (last-7-days-start (ts-adjust 'day -6 today-start))
         (last-30-days-start (ts-adjust 'day -30 today-start))
         (last-year-start (ts-adjust 'month -11 today-start))
         (created-at (oref conversation :created-at))
         (section-info (cond
                        ((ts>= created-at today-start) '("Today" 1000004))
                        ((ts>= created-at yesterday-start) '("Yesterday" 1000003))
                        ((ts>= created-at last-7-days-start) '("Last 7 days" 1000002))
                        ((ts>= created-at last-30-days-start) '("Last 30 days" 1000001))
                        ((ts>= created-at last-year-start)
                         `(,(ts-month-name created-at)
                           ,(string-to-number (ts-format "%Y%m" created-at))))
                        (t '("History" 0)))))
    (ConversationSection
     :name (nth 0 section-info)
     :priority (nth 1 section-info))
    ))

(defun aibo:--get-conversation-sections (conversations)
  (let ((sections nil))
    ;; Add conversations to their respective sections
    (--each conversations
      (let* ((conversation it)
             (section (ConversationSection-from conversation))
             (section-index (--find-index (eq (oref it :name) (oref section :name)) sections)))
        ;; Make sure the section exists
        (if (eq section-index nil)
            (progn
              (setq section-index 0)
              (push section sections)))
        ;; Add the conversation to its section
        (push conversation (oref (nth section-index sections) :conversations))))

    ;; Order conversations by :created-at
    (--each sections
      (let* ((section it)
             (conversations (oref section :conversations)))
        (oset section :conversations
              (--sort
               (ts> (oref it :created-at) (oref other :created-at))
               conversations))))

    ;; Order sections by priority
    (--sort
     (> (oref it :priority) (oref other :priority))
     sections)))

;; ---[ ConversationTemplate ]--------------------
(defclass ConversationTemplate ()
  ((name
    :documentation "The human-readable name"
    :initarg :name
    :type string)
   (short-name
    :documentation "Short name for starting a conversation quickly"
    :initarg :short-name
    :type string)
   (message-inputs
    :documentation "Message inputs used to create the new conversation"
    :initarg :message-inputs)))

(defclass CreateMessageInput ()
  ((role
    :documentation "Message role"
    :initarg :role
    :type string)
   (content
    :documentation "Message content type"
    :initarg :content)))

(cl-defmethod CreateMessageInput-to-api ((message-input CreateMessageInput))
  `(("role" . ,(oref message-input :role))
    ("content" . ,(oref message-input :content))))

(provide 'aibo-types)
