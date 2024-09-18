;;; aibo-types.el --- Most of the eieio class definitions -*- lexical-binding: t -*-
(require 'dash)
(require 'eieio)
(require 'ht)
(require 'ts)

;; ---[ ConversationSection ]---------------------
(defclass aibo:ConversationSection ()
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

(defun aibo:ConversationSection-from (conversation)
  (let* ((now (ts-now))
         (today-start        (ts-apply :hour 0 :minute 0 :second 0 now))
         (yesterday-start    (ts-adjust 'day -1 today-start))
         (prev-7-days-start  (ts-adjust 'day -6 today-start))
         (prev-30-days-start (ts-adjust 'day -30 today-start))
         (prev-year-start    (ts-adjust 'month -11 today-start))
         (created-at (ts-parse (ht-get conversation "created_at")))
         (section-info (cond
                        ((ts>= created-at today-start)        '("Today" 1000004))
                        ((ts>= created-at yesterday-start)    '("Yesterday" 1000003))
                        ((ts>= created-at prev-7-days-start)  '("Previous 7 days" 1000002))
                        ((ts>= created-at prev-30-days-start) '("Previous 30 days" 1000001))
                        ((ts>= created-at prev-year-start)
                         `(,(ts-month-name created-at)
                           ,(string-to-number (ts-format "%Y%m" created-at))))
                        (t
                         `(,(format "%s - %s" (ts-year created-at) (ts-month-name created-at))
                           ,(ts-year created-at))))))
    (aibo:ConversationSection
     :name (nth 0 section-info)
     :priority (nth 1 section-info))))

(defun aibo:--get-conversation-sections (conversations)
  (let ((sections nil))
    ;; Add conversations to their respective sections
    (--each conversations
      (let* ((conversation it)
             (section (aibo:ConversationSection-from conversation))
             (section-index (--find-index (string= (oref it :name) (oref section :name)) sections)))
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
               (string> (ht-get it "created_at") (ht-get other "created_at"))
               conversations))))

    ;; Order sections by priority
    (--sort
     (> (oref it :priority) (oref other :priority))
     sections)))

;; ---[ ConversationTemplate ]--------------------
(defclass aibo:ConversationTemplate ()
  ((name
    :documentation "The human-readable name"
    :initarg :name
    :type string)
   (short-name
    :documentation "Short name for starting a conversation quickly"
    :initarg :short-name
    :type string)
   (model
    :documentation "The model to sample from"
    :initarg :model
    :initform nil)
   (action-type
    :documentation "Action taken on selection: :new-conversation or :buffer-insert"
    :initarg :action-type)
   (get-message-inputs
    :documentation "Function to generate message inputs based on the input"
    :initarg :get-message-inputs
    :initform nil)
   (message-inputs
    :documentation "Static message inputs"
    :initarg :message-inputs
    :initform nil)))

(provide 'aibo-types)
