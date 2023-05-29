;;; aibo-templates.el --- Templates for quick conversation creation -*- lexical-binding: t -*-
(provide 'aibo-types)
(require 's)
(require 'dash)

(setq aibo:--generic-system-message
      "You are an AI with all the knowledge available in the world up to your knowledge cutoff. You give concise and succinct answers to anything asked, only providing deep analysis, relevant examples, and supporting details when asked or absolutely necessary. Prioritize conciseness and succinctness.")

(setq aibo:conversation-templates
      `(
        ;; ---[ Question ]--------------------------
        ,(ConversationTemplate
         :short-name "q"
         :name "Question"
         :message-inputs
         `(,(CreateMessageInput
            :role "system"
            :content `(("kind" . "text")
                       ("text" . ,aibo:--generic-system-message)))
           ,(CreateMessageInput
            :role "user"
            :content `(("kind" . "text")
                       ("text" .
                        "@@@content@@@")))))
        ;; ---[ Write documentation ]---------------
        ,(ConversationTemplate
         :short-name "d"
         :name "Write docstrings"
         :message-inputs
         `(,(CreateMessageInput
            :role "system"
            :content `(("kind" . "text")
                       ("text" . ,aibo:--generic-system-message)))
           ,(CreateMessageInput
            :role "user"
            :content `(("kind" . "text")
                       ("text" .
                        "Can you convert this function to include docstrings explaining the args and return value? Please make it as concise and succinct as possible and avoid verbosity:\n\n@@@content@@@")))))
        ;; ---[ Spellcheck ]---------------
        ,(ConversationTemplate
         :short-name "s"
         :name "Spellcheck"
         :message-inputs
         `(,(CreateMessageInput
            :role "system"
            :content `(("kind" . "text")
                       ("text" . ,aibo:--generic-system-message)))
           ,(CreateMessageInput
            :role "user"
            :content `(("kind" . "text")
                       ("text" .
                        "Can you spellcheck and review the grammar for this with minimal changes?:\n\n@@@content@@@")))))
        ))

(defun aibo:get-conversation-template (&rest args)
  (let* ((short-name (plist-get args :short-name)))
    (--first
     (string= (oref it :short-name) short-name)
     aibo:conversation-templates)))


(defun aibo:get-conversation-template-message-inputs (&rest args)
  (let* ((template (plist-get args :template))
         (content (plist-get args :content))
         (message-inputs (oref template :message-inputs))
         (region-regex "\\(^\\|\\s-\\)\\\\r\\(\\s-\\|$\\)")
         (buffer-regex "\\(^\\|\\s-\\)\\\\b\\(\\s-\\|$\\)")
         (region-string (if (and transient-mark-mode mark-active)
                            (buffer-substring (region-beginning) (region-end))
                          ""))
         (content (if (s-contains? "\\r" content)
                      (replace-regexp-in-string
                       region-regex
                       (if (> (length region-string) 0)
                           (format "\n\n----------\n%s\n----------\n\n" region-string)
                         " ")
                       content
                       t t)
                    content))
         (content (if (s-contains? "\\b" content)
                      (replace-regexp-in-string
                       buffer-regex
                       (format "\n\n----------\n%s\n----------\n\n" (buffer-string))
                       content
                       t t)
                    content))
         (last-message-input (car (last message-inputs)))
         (last-message-input-content (oref last-message-input :content))
         (last-message-input-kind (cdr (assoc "kind" last-message-input-content)))
         (last-message-input-text (cdr (assoc "text" last-message-input-content)))
         (last-message-input-user-text
          (s-replace "@@@content@@@" content last-message-input-text)))
    (append
     (butlast message-inputs)
     `(,(CreateMessageInput
         :role (oref last-message-input :role)
         :content `(("kind" . ,last-message-input-kind)
                    ("text" . ,last-message-input-user-text)))))))

(provide 'aibo-templates)
