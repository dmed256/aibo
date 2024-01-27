;;; aibo-commands.el --- Standalone commands that can be run anywhere -*- lexical-binding: t -*-

(require 'aibo-templates)
(require 'aibo-conversation)

(defun aibo:rephrase-yank ()
  "Returns a list of package info: name + is-enabled"
  (interactive)
  (let* ((clipboard (substring-no-properties (current-kill 0)))
         (template
          (aibo:ConversationTemplate
           :short-name "cmy"
           :name "Yank++"
           :action-type :buffer-stream-insert
           :get-message-inputs
           (lambda (content)
             `(,(ht ("role"     "system")
                    ("contents" `(,(ht ("kind" "text")
                                       ("text" aibo:--generic-system-message)))))
               ,(ht ("role"     "user")
                    ("contents" `(,(ht ("kind" "text")
                                       ("text" (concat
                                                "--------------------\n"
                                                clipboard "\n"
                                                "--------------------\n\n"
                                                "Rewrite the above given the instructions: " content)))))))))))

    (aibo:--create-conversation-from-template template)))

(provide 'aibo-commands)
