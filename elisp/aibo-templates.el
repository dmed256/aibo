;;; aibo-templates.el --- Templates for quick conversation creation -*- lexical-binding: t -*-
(provide 'aibo-custom)
(provide 'aibo-types)

(require 'ht)
(require 's)
(require 'dash)

(setq aibo:--generic-system-message
      "You are an AI with all the knowledge available in the world up to your knowledge cutoff date. You give correct, concise, and succinct answers to anything asked, only providing deep analysis, relevant examples, and supporting details when asked or absolutely necessary. Prioritize correctness, conciseness, and succinctness. I'm an expert programmer so only give short answers without code explanations unless explicitly asked.")


(setq aibo:base-conversation-templates
      `(
        ;; ---[ Question ]--------------------------
        ,(aibo:ConversationTemplate
          :short-name "q"
          :name "Question"
          :action-type :new-conversation
          :get-message-inputs
          (lambda (content)
            `(,(ht ("role"     "system")
                   ("contents" `(,(ht ("kind" "text")
                                      ("text" aibo:--generic-system-message)))))
              ,(ht ("role"     "user")
                   ("contents" `(,(ht ("kind" "text")
                                      ("text" content))))))))
        ;; ---[ Command ]---------------------------
        ,(aibo:ConversationTemplate
          :short-name "c"
          :name "Command find"
          :action-type :new-conversation
          :get-message-inputs
          (lambda (content)
            `(,(ht ("role"     "system")
                   ("contents" `(,(ht ("kind" "text")
                                      ("text" aibo:--generic-system-message)))))
              ,(ht ("role"     "user")
                   ("contents" `(,(ht ("kind" "text")
                                      ("text" (concat
                                               "Can you give me a command which answers the following:\n"
                                               "\n"
                                               content "\n"
                                               "\n"
                                               "Only the command and nothing more, no ``` either")))))))))
        ;; ---[ Spellcheck ]---------------
        ,(aibo:ConversationTemplate
          :short-name "s"
          :name "Spellcheck"
          :action-type :new-conversation
          :get-message-inputs
          (lambda (content)
            `(,(ht ("role"     "system")
                   ("contents" `(,(ht ("kind" "text")
                                      ("text" aibo:--generic-system-message))))))
            ,(ht ("role"     "user")
                 ("contents" `(,(ht ("kind" "text")
                                    ("text" (concat "Can you spellcheck and review the grammar for this with minimal changes?:\n"
                                                    "\n"
                                                    content))))))))
        ;; ---[ Copilot ]------------------
        ,(aibo:ConversationTemplate
          :short-name "cp"
          :name "Copilot"
          :action-type :buffer-insert
          :get-message-inputs
          (lambda (content)
            (let* ((prefix (buffer-substring (point-min) (point)))
                   (suffix (buffer-substring (point) (point-max))))
              `(,(ht ("role"     "system")
                     ("contents" `(,(ht ("kind" "text")
                                        ("text" "You are a helpful copilot code-generation AI that auto-completes missing code where @@CODE INJECTION SITE@@ is located.")))))
                ,(ht ("role"     "user")
                     ("contents" `(,(ht ("kind" "text")
                                        ("text" (concat
                                                 "Here are the current full file contents:\n"
                                                 "----------------\n"
                                                 prefix "@@CODE INJECTION SITE@@" suffix "\n"
                                                 "----------------\n"
                                                 "\n"
                                                 "To help spot where I'll like your help, here's the code snippet from the file above where you'll inject the code:\n"
                                                 "----------------\n"
                                                 "..." (substring prefix (max 0 (- (length prefix) 250)))
                                                 "@@CODE INJECTION SITE@@"
                                                 (substring suffix 0 (min 250 (length suffix))) "...\n"
                                                 "----------------\n"
                                                 "\n"
                                                 "Output ONLY what should be injected, which should be related to: " content "\n"))))))))))
        ))

(defun aibo:conversation-templates ()
  (append aibo:base-conversation-templates aibo:custom-conversation-templates))

(defun aibo:get-conversation-template (&rest args)
  (let* ((short-name (plist-get args :short-name)))
    (--first
     (string= (oref it :short-name) short-name)
     (aibo:conversation-templates))))


(defun aibo:get-conversation-template-message-inputs (&rest args)
  (let* ((template (plist-get args :template))
         (content (plist-get args :content))
         (get-message-inputs (oref template :get-message-inputs)))
    (funcall get-message-inputs content)))

(provide 'aibo-templates)
