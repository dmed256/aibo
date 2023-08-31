;;; aibo-templates.el --- Templates for quick conversation creation -*- lexical-binding: t -*-
(provide 'aibo-types)
(require 's)
(require 'dash)

(setq aibo:--generic-system-message
      "You are an AI with all the knowledge available in the world up to your knowledge cutoff date. You give correct, concise, and succinct answers to anything asked, only providing deep analysis, relevant examples, and supporting details when asked or absolutely necessary. Prioritize correctness, conciseness, and succinctness.")

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
        ,(aibo:ConversationTemplate
          :short-name "q"
          :name "Question"
          :action-type :new-conversation
          :get-message-inputs
          (lambda (content)
            `(,(aibo:CreateMessageInput
                :role "system"
                :content `(("kind" . "text")
                           ("text" . ,aibo:--generic-system-message)))
              ,(aibo:CreateMessageInput
                :role "user"
                :content `(("kind" . "text")
                           ("text" . ,(aibo:--expand-conversation-template-shorthands content)))))))
        ;; ---[ Command ]---------------------------
        ,(aibo:ConversationTemplate
          :short-name "c"
          :name "Command find"
          :action-type :new-conversation
          :get-message-inputs
          (lambda (content)
            `(,(aibo:CreateMessageInput
                :role "system"
                :content `(("kind" . "text")
                           ("text" . ,aibo:--generic-system-message)))
              ,(aibo:CreateMessageInput
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
        ,(aibo:ConversationTemplate
          :short-name "d"
          :name "Write docstrings"
          :action-type :new-conversation
          :get-message-inputs
          (lambda (content)
            `(,(aibo:CreateMessageInput
                :role "system"
                :content `(("kind" . "text")
                           ("text" . ,aibo:--generic-system-message)))
              ,(aibo:CreateMessageInput
                :role "user"
                :content `(("kind" . "text")
                           ("text" .
                            ,(concat
                              "Can you convert this function to include docstrings explaining the args and return value?"
                              " Please make it as concise and succinct as possible and avoid verbosity:\n"
                              "\n"
                              (aibo:--expand-conversation-template-shorthands content))))))))
        ;; ---[ Spellcheck ]---------------
        ,(aibo:ConversationTemplate
          :short-name "s"
          :name "Spellcheck"
          :action-type :new-conversation
          :get-message-inputs
          (lambda (content)
            `(,(aibo:CreateMessageInput
                :role "system"
                :content `(("kind" . "text")
                           ("text" . ,aibo:--generic-system-message)))
              ,(aibo:CreateMessageInput
                :role "user"
                :content `(("kind" . "text")
                           ("text" .
                            ,(concat "Can you spellcheck and review the grammar for this with minimal changes?:\n"
                                     "\n"
                                     (aibo:--expand-conversation-template-shorthands content))))))))
        ;; ---[ File info ]----------------
        ,(aibo:ConversationTemplate
          :short-name "f"
          :name "File info"
          :action-type :new-conversation
          :get-message-inputs
          (lambda (content)
            `(,(aibo:CreateMessageInput
                :role "system"
                :content `(("kind" . "text")
                           ("text" . "You are a helpful AI that generates structured file information for the given file.")))
              ,(aibo:CreateMessageInput
                :role "user"
                :content `(("kind" . "text")
                           ("text" .
                            ,(concat
                              (buffer-string)
                              "\n"
                              "--------------------\n"
                              "\n"
                              "Please give me FileInfo for the file contents above\n"
                              "\n"
                              "type FileInfo {\n"
                              "    // File name\n"
                              "    filename: \"" (buffer-file-name) "\";\n"
                              "    // Programming language\n"
                              "    language: string | null;\n"
                              "    // Summary of the file\n"
                              "    summary: string;\n"
                              "    // Imports used\n"
                              "    imports: Import[];\n"
                              "    // Functions defined in the file\n"
                              "    functions: Function[];\n"
                              "}\n"
                              "\n"
                              "type Import {\n"
                              "    // Fully qualified name, such as \"http.server\"\n"
                              "    name: string;\n"
                              "    // Imported things\n"
                              "    imports: string[];\n"
                              "}\n"
                              "\n"
                              "type Function {\n"
                              "    // Fully qualified name, such as \"my_module.class_name.function_name\"\n"
                              "    name: string;\n"
                              "    // Summary of what the function is for\n"
                              "    summary: string;\n"
                              "    arguments: {\n"
                              "        name: string;\n"
                              "        type: string;\n"
                              "        summary: string;\n"
                              "    }[]\n"
                              "}\n"
                              )))))))
        ;; ---[ Copilot ]------------------
        ,(aibo:ConversationTemplate
          :short-name "cp"
          :name "Copilot"
          :action-type :buffer-insert
          :get-message-inputs
          (lambda (content)
            (let* ((prefix (buffer-substring (point-min) (point)))
                   (suffix (buffer-substring (point) (point-max))))
              `(,(aibo:CreateMessageInput
                  :role "system"
                  :content `(("kind" . "text")
                             ("text" . "You are a helpful copilot code-generation AI that auto-completes missing code where @@CODE INJECTION SITE@@ is located.")))
                ,(aibo:CreateMessageInput
                  :role "user"
                  :content `(("kind" . "text")
                             ("text" .
                              ,(concat
                                "Here are the current full file contents:\n"
                                "----------------\n"
                                prefix "@@CODE INJECTION SITE@@" suffix "\n"
                                "----------------\n"
                                "\n"
                                "To help spot where I'll like your help, here's the code snippet from the file above where you'll inject the code:\n"
                                "----------------\n"
                                "..." (substring prefix (max 0 (- (length prefix) 100)))
                                "@@CODE INJECTION SITE@@"
                                (substring suffix 0 (min 100 (length suffix))) "...\n"
                                "----------------\n"
                                "\n"
                                "Output ONLY what should be injected, which should be related to: " content "\n"))))))))
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
