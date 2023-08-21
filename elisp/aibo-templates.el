;;; aibo-templates.el --- Templates for quick conversation creation -*- lexical-binding: t -*-
(provide 'aibo-types)
(require 's)
(require 'dash)

(setq aibo:--generic-system-message
      "You are an AI with all the knowledge available in the world up to your knowledge cutoff. You give concise and succinct answers to anything asked, only providing deep analysis, relevant examples, and supporting details when asked or absolutely necessary. Prioritize conciseness and succinctness.")

(setq aibo:--copilot-system-message
      (concat
       "You are a helpful copilot code-generation AI that auto-completes code where [USER-CURSOR] is located.\n"
       "- Inspect the given file contents.\n"
       "- Output ONLY the code that will be literally placed where [USER-CURSOR] is located, so don't output nearby code since it'll be duplicated.\n"
       "- Leverage all the code in the file, especially nearby code and comments, to output the code that will be inserted.\n"
       "- Use correct newlines and indentation so that replacing where [USER-CURSOR] is located will be syntactically correct."))

(defun aibo:--expand-conversation-template-shorthands (text)
  "Replace \b -> Buffer and \r -> Region"
  (let* ((header (make-string 20 ?-))
         (region-regex "\\(^\\|\\s-\\)\\\\r\\(\\s-\\|$\\)")
         (buffer-regex "\\(^\\|\\s-\\)\\\\b\\(\\s-\\|$\\)")
         (region-string (if (mark)
                            (buffer-substring (min (mark) (point)) (max (mark) (point)))
                          ""))
         (text (if (s-contains? "\\r" text)
                   (replace-regexp-in-string
                    region-regex
                    (if (> (length region-string) 0)
                        (format "\n\n%s\n%s\n%s\n\n" header region-string header)
                      " ")
                    text
                    t t)
                 text))
         (text (if (s-contains? "\\b" text)
                   (replace-regexp-in-string
                    buffer-regex
                    (format "\n\n%s\n%s\n%s\n\n" header (buffer-string) header)
                    text
                    t t)
                 text)))
    text))


(setq aibo:conversation-templates
      `(
        ;; ---[ Question ]--------------------------
        ,(ConversationTemplate
          :short-name "q"
          :name "Question"
          :action-type :new-conversation
          :get-message-inputs
          (lambda (content)
            `(,(CreateMessageInput
                :role "system"
                :content `(("kind" . "text")
                           ("text" . ,aibo:--generic-system-message)))
              ,(CreateMessageInput
                :role "user"
                :content `(("kind" . "text")
                           ("text" . ,(aibo:--expand-conversation-template-shorthands content)))))))
        ;; ---[ Command ]---------------------------
        ,(ConversationTemplate
          :short-name "c"
          :name "Command find"
          :action-type :new-conversation
          :get-message-inputs
          (lambda (content)
            `(,(CreateMessageInput
                :role "system"
                :content `(("kind" . "text")
                           ("text" . ,aibo:--generic-system-message)))
              ,(CreateMessageInput
                :role "user"
                :content `(("kind" . "text")
                           ("text" .
                            ,(concat
                              "Can you give me a command which answers the following:\n"
                              "\n"
                              (aibo:--expand-conversation-template-shorthands content) "\n"
                              "\n"
                              "Only the command and nothing more, no ``` either")))))))
        ;; ---[ Write documentation ]---------------
        ,(ConversationTemplate
          :short-name "d"
          :name "Write docstrings"
          :action-type :new-conversation
          :get-message-inputs
          (lambda (content)
            `(,(CreateMessageInput
                :role "system"
                :content `(("kind" . "text")
                           ("text" . ,aibo:--generic-system-message)))
              ,(CreateMessageInput
                :role "user"
                :content `(("kind" . "text")
                           ("text" .
                            ,(concat
                              "Can you convert this function to include docstrings explaining the args and return value?"
                              " Please make it as concise and succinct as possible and avoid verbosity:\n"
                              "\n"
                              (aibo:--expand-conversation-template-shorthands content))))))))
        ;; ---[ Spellcheck ]---------------
        ,(ConversationTemplate
          :short-name "s"
          :name "Spellcheck"
          :action-type :new-conversation
          :get-message-inputs
          (lambda (content)
            `(,(CreateMessageInput
                :role "system"
                :content `(("kind" . "text")
                           ("text" . ,aibo:--generic-system-message)))
              ,(CreateMessageInput
                :role "user"
                :content `(("kind" . "text")
                           ("text" .
                            ,(concat "Can you spellcheck and review the grammar for this with minimal changes?:\n"
                                     "\n"
                                     (aibo:--expand-conversation-template-shorthands content))))))))
        ;; ---[ Copilot ]------------------
        ,(ConversationTemplate
          :short-name "cp"
          :name "Copilot"
          :action-type :buffer-insert
          :get-message-inputs
          (lambda (content)
            (let* ((pre-point-buffer-content (buffer-substring (point-min) (point)))
                   (post-point-buffer-content (buffer-substring (point) (point-max))))
              `(,(CreateMessageInput
                  :role "system"
                  :content `(("kind" . "text")
                             ("text" . ,aibo:--copilot-system-message)))
                ,(CreateMessageInput
                  :role "user"
                  :content `(("kind" . "text")
                             ("text" .
                              ,(concat
                                "# File\n"
                                "----------------\n"
                                pre-point-buffer-content "[USER-CURSOR]" post-point-buffer-content "\n"
                                "----------------\n"
                                "\n"
                                "As a reminder, " aibo:--copilot-system-message "\n"
                                "I'll copy-paste exactly what you output where [USER-CURSOR] is located, so only output the code as a continuation of the code without explanations."))))))))
        ))

(defun aibo:get-conversation-template (&rest args)
  (let* ((short-name (plist-get args :short-name)))
    (--first
     (string= (oref it :short-name) short-name)
     aibo:conversation-templates)))


(defun aibo:get-conversation-template-message-inputs (&rest args)
  (let* ((template (plist-get args :template))
         (content (plist-get args :content))
         (get-message-inputs (oref template :get-message-inputs)))
    (funcall get-message-inputs content)))

(provide 'aibo-templates)
