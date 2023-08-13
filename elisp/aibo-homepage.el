;;; aibo-homepage.el --- Methods related to the homepage view which lists historical conversations -*- lexical-binding: t -*-
(require 's)
(require 'dash)
(require 'ivy)
(require 'widget)

(require 'aibo-api)
(require 'aibo-utils)
(require 'aibo-templates)

(setq aibo:homepage-buffer-name "*Aibo homepage*")

(defun aibo:--ansi-buffer (beg end len)
  (interactive)
  (ansi-color-apply-on-region beg end))

(defun aibo:homepage ()
  (interactive)
  (aibo:--get-or-create-buffer
   :name aibo:homepage-buffer-name
   :open-style :new-window
   :on-load (lambda (buffer)
              (aibo:mode)
              (aibo:refresh-homepage))))

(defun aibo:refresh-homepage (&rest args)
  (interactive)
  (aibo:api-get-conversations
   :on-success
   (lambda (conversations)
     (with-current-buffer (get-buffer aibo:homepage-buffer-name)
       (let* ((current-point (point))
              (sections (aibo:--get-conversation-sections conversations)))

         (let ((inhibit-read-only t))
           (erase-buffer))
         (remove-overlays)

         (if sections
             (--each-indexed sections
               (aibo:--render-conversation-section it))
           (widget-insert (propertize
                           "No conversations"
                           'font-lock-face '(:foreground "#9c9c9c"))))

         (widget-setup)

         (not-modified)
         (goto-char current-point))))))

(defun aibo:--render-conversation-section (section)
  (let ((conversations (oref section :conversations)))
    ;; Add the section title
    (widget-insert (propertize
                    (format "%s\n" (oref section :name))
                    'font-lock-face '(:foreground "#9c9c9c")))

    ;; Add conversation titles for the section
    (--each-indexed conversations
      (let* ((conversation it))

        (widget-insert "  ")

        ;; [DEL]
        (widget-create
         'link
         :notify (lambda (&rest ignore)
                   (let* ((id (oref conversation :id))
                          (title (oref conversation :title))
                          (conversation-buffer (get-buffer
                                                (aibo:--get-conversation-buffer-name
                                                 :id id
                                                 :title title))))

                     (aibo:api-delete-conversation
                      :id id
                      :on-success
                      (lambda (_)
                        (if conversation-buffer
                            (kill-buffer conversation-buffer))
                        (aibo:refresh-homepage)))))
         :button-prefix "["
         :button-suffix "] "
         :tag "DEL")

        ;; [EDIT]
        (widget-create
         'link
         :notify (lambda (&rest ignore)
                   (aibo:set-conversation-title conversation))
         :button-prefix "["
         :button-suffix "] "
         :tag "EDIT")

        ;; Conversation title
        (widget-create
         'link
         :notify (lambda (&rest _ignore)
                   (aibo:go-to-conversation-by-id (oref conversation :id)))
         :button-prefix ""
         :button-suffix ""
         :tag (aibo:--stylize-conversation-title
               (oref conversation :title)))))))

(defun aibo:--stylize-conversation-title (title)
  (let* ((parts (s-split " " title))
         (stylized-parts
          (--map
           (let* ((part it)
                  (is-tag? (s-starts-with? "#" part))
                  (color (if is-tag? "#8cc4ff" "#e9b96e")))
             (propertize part 'face `(:foreground ,color)))
           parts)))
    (concat (s-join " " stylized-parts) "\n")))

(provide 'aibo-homepage)
