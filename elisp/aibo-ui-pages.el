;;; aibo-ui-pages.el --- Home and reference pages -*- lexical-binding: t -*-

(require 'aibo-ui-base)
(defvar aibo:chat-row-map)
(declare-function aibo:open-link "aibo-ui-messages")
(declare-function aibo:chat-mode "aibo-ui-chat")
(declare-function aibo:render-sidebar "aibo-ui-sidebar")
(declare-function aibo:--echo "aibo-ui-input")
(declare-function aibo:--full-layout "aibo-ui-layout")

(defun aibo:--insert-chat-row (chat)
  (let ((start (point)))
    (insert (format "  %s  %s\n" (aibo:--badge chat t) (aibo:--row-title chat t)))
    (add-text-properties
     start (point)
     `(aibo-chat-id ,(aibo:--get chat "id")
                    keymap ,aibo:chat-row-map
                    mouse-face highlight
                    help-echo "RET: open chat"))
    (when (aibo:--active-p chat)
      (add-face-text-property start (point) (aibo:--message-face chat) t)
      (put-text-property start (point) 'line-prefix
                         (propertize " " 'face (aibo:--message-face chat))))))

(defun aibo:--project-chats (project-id)
  (seq-filter #'aibo:--public-chat-p
              (if (hash-table-p aibo:home-groups)
                  (gethash project-id aibo:home-groups)
                (seq-filter (lambda (chat)
                              (equal (aibo:--get (aibo:--get chat "project") "id") project-id))
                            aibo:chats))))

(defun aibo:--fetch-home-data (on-success on-error)
  "Fetch complete workspace data, including each independent project group."
  (let ((pending 5) failed
        (data (make-hash-table :test #'equal))
        (groups (make-hash-table :test #'equal)))
    (puthash "groups" groups data)
    (cl-labels ((done ()
                  (cl-decf pending)
                  (when (and (= pending 0) (not failed)) (funcall on-success data)))
                (loaded (key value) (puthash key value data) (done))
                (reject (error)
                  (unless failed (setq failed t) (funcall on-error error))))
      (aibo:api-get-chats (lambda (chats) (loaded "chats" chats)) nil 100 nil #'reject)
      (aibo:api-get-locations (lambda (locations) (loaded "locations" locations)) #'reject)
      (aibo:api-get-sidebar (lambda (sidebar) (loaded "sidebar" sidebar)) #'reject)
      (aibo:api-get-projects (lambda (archived) (loaded "archived" archived)) t #'reject)
      (aibo:api-get-projects
       (lambda (projects)
         (cl-incf pending (1+ (length projects)))
         (dolist (id (cons nil (mapcar (lambda (project) (aibo:--get project "id")) projects)))
           (aibo:api-get-project-chats id
                                       (lambda (chats) (puthash id chats groups) (done)) #'reject))
         (loaded "projects" projects)) nil #'reject))))

(defun aibo:--render-home (archived-projects)
  (let ((buffer (get-buffer-create aibo:home-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'aibo:chat-mode) (aibo:chat-mode))
      (setq-local left-margin-width 0 line-prefix " " wrap-prefix " ")
      (let ((inhibit-read-only t))
        (aibo:--preserve-view
         (erase-buffer)
         (insert "\n")
         (dolist (chat (seq-take (aibo:--project-chats nil) 10))
           (aibo:--insert-chat-row chat))
         (dolist (project (sort (copy-sequence aibo:projects)
                                (lambda (a b) (string> (aibo:--get a "name") (aibo:--get b "name")))))
           (unless (= (point) (1+ (point-min))) (insert "\n"))
           (insert (propertize
                    (format "# %s\n\n" (aibo:--get project "name"))
                    'face 'aibo:orange-face))
           (dolist (chat (seq-take
                          (aibo:--project-chats (aibo:--get project "id")) 10))
             (aibo:--insert-chat-row chat)))
         (when archived-projects (insert "\n"))
         (dolist (project archived-projects)
           (insert (propertize (concat (aibo:--get project "name") "\n")
                               'face 'aibo:muted-face))))))
    (when (eq aibo:page 'home) (aibo:--full-layout buffer))))

(defun aibo:--home-status (state)
  "Render a loading or retry page while retaining the composer and windows."
  (let ((buffer (get-buffer-create aibo:home-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'aibo:chat-mode) (aibo:chat-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (if (eq state 'loading) "Loading…\n\n" "Workspace unavailable\n\n")
                            'face 'aibo:orange-face))
        (insert (propertize (if (eq state 'loading) "Connecting to your workspace.\n"
                              "Could not load this page. Your draft is preserved.\n")
                            'face 'aibo:muted-face))
        (when (eq state 'error)
          (insert-text-button "[Retry]" 'face 'aibo:link-face 'follow-link t
                              'action (lambda (_) (aibo:homepage)))
          (insert "\n"))
        (goto-char (point-min))))
    (unless aibo:sidebar (aibo:render-sidebar (make-hash-table)))
    (aibo:--full-layout buffer)))

(defun aibo:--commit-home (data generation)
  (when (and (= generation aibo:home-generation) (eq aibo:page 'home))
    (setq aibo:chats (gethash "chats" data) aibo:projects (gethash "projects" data)
          aibo:locations (gethash "locations" data) aibo:home-groups (gethash "groups" data))
    (aibo:render-sidebar (gethash "sidebar" data))
    (aibo:--render-home (gethash "archived" data))))

(defun aibo:--home-error (generation)
  (when (and (= generation aibo:home-generation) (eq aibo:page 'home))
    (aibo:--home-status 'error)
    (aibo:--echo "Workspace unavailable · retry when connected" t)))

(defun aibo:homepage ()
  (interactive)
  (setq aibo:page 'home aibo:current-chat nil aibo:opening-chat-id nil)
  (unless (eq aibo:layout 'full)
    (setq aibo:pre-full-window-configuration (current-window-configuration)))
  (setq aibo:layout 'full)
  (if (and (hash-table-p aibo:home-groups) (get-buffer aibo:home-buffer))
      (aibo:--full-layout (get-buffer aibo:home-buffer))
    (aibo:--home-status 'loading))
  (let ((generation (cl-incf aibo:home-generation)))
    (aibo:--fetch-home-data (lambda (data) (aibo:--commit-home data generation))
                            (lambda (_) (aibo:--home-error generation))))
  (condition-case error
      (aibo:api-connect-events)
    (error (message "%s" (error-message-string error)))))

(defun aibo:--show-page (name render)
  (setq aibo:page name aibo:current-chat nil aibo:opening-chat-id nil)
  (let ((buffer (get-buffer-create (format "*Aibo %s*" name))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'aibo:chat-mode) (aibo:chat-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "# %s\n\n" (capitalize (symbol-name name)))
                            'face 'aibo:orange-face))
        (funcall render)
        (goto-char (point-min))))
    (if (eq aibo:layout 'full) (aibo:--full-layout buffer) (switch-to-buffer buffer)))
  (when aibo:sidebar (aibo:render-sidebar aibo:sidebar)))

(defun aibo:locations-page ()
  (interactive)
  (aibo:--show-page 'locations (lambda () (insert "Loading locations…\n")))
  (aibo:api-get-locations
   (lambda (locations)
     (setq aibo:locations locations)
     (when (eq aibo:page 'locations)
       (aibo:--show-page
        'locations
        (lambda ()
          (insert (propertize "NAME            WORKING DIRECTORY\n\n" 'face 'aibo:muted-face))
          (dolist (location locations)
            (insert (format "%-16s%s\n" (aibo:--get location "name")
                            (aibo:--get location "path"))))
          (unless locations
            (insert (propertize "No locations configured.\n" 'face 'aibo:muted-face)))))))))

(defun aibo:projects-page ()
  (interactive)
  (aibo:--show-page 'projects (lambda () (insert "Loading projects…\n")))
  (aibo:api-get-projects
   (lambda (projects)
     (setq aibo:projects projects)
     (aibo:api-get-projects
      (lambda (archived)
        (when (eq aibo:page 'projects)
          (aibo:--show-page
           'projects
           (lambda ()
             (dolist (project projects)
               (let ((name (aibo:--get project "name")))
                 (insert (propertize name 'face 'aibo:orange-face)
                         "\n  " (aibo:--get project "description") "\n  ")
                 (insert-text-button
                  (format "~/.cache/aibo/projects/%s/README.md" name)
                  'face 'aibo:link-face 'follow-link t
                  'aibo-path (expand-file-name (format "~/.cache/aibo/projects/%s/README.md" name))
                  'action #'aibo:open-link)
                 (insert "\n\n")))
             (when archived
               (insert (propertize "# Archived\n\n" 'face 'aibo:orange-face))
               (dolist (project archived)
                 (insert (propertize (concat (aibo:--get project "name") "\n")
                                     'face 'aibo:muted-face))))
             (unless (or projects archived)
               (insert (propertize "No projects configured.\n" 'face 'aibo:muted-face))))))) t))))

(defun aibo:help ()
  (interactive)
  (aibo:--show-page
   'help
   (lambda ()
     (dolist (binding '(
                        ("C-M-h" "Home / return to full mode")
                        ("M-0" "New m# chat")
                        ("M-1 … M-9" "Open b# from the purple bar")
                        ("C-c b 0 … 9" "Open m# from the orange bar")
                        ("C-c b" "Search chats (cowork mode)")
                        ("C-c p s" "Search chats in either mode")
                        ("C-c n 0 … f" "Open a notification")
                        ("C-c p n" "Focus and scroll notifications")
                        ("C-c p l" "Locations")
                        ("C-c p p" "Projects")
                        ("C-c p c" "Customization")
                        ("C-c p h" "Help")
                        ("M-/" "Focus input; toggle chat / new draft")
                        ("C-o" "Cycle content / input; skip sidebar")
                        ("RET" "Newline in input; activate links/buttons")
                        ("M-RET" "Send draft")
                        ("C-g" "Clear; also close input in cowork")
                        ("C-c C-i" "Attach clipboard image")
                        ("M-w" "Copy literal selected text")
                        ("M-m" "Toggle raw / pretty message at point")
                        ("C-x 0/1/2/3" "Leave full mode; normal window command")))
       (insert (propertize (format "%-20s" (car binding)) 'face 'aibo:key-face)
               "  " (truncate-string-to-width (cadr binding) (max 1 (- (aibo:--content-width) 24)))
               "\n")))))

(defun aibo:customization ()
  "Open Aibo's shared customization settings."
  (interactive)
  (aibo:--show-page 'customization (lambda () (insert "Loading customization…\n")))
  (aibo:api--request
   "GET" "/api/settings/models" nil
   (lambda (settings)
     (when (eq aibo:page 'customization)
       (aibo:--show-page
        'customization
        (lambda ()
          (insert (propertize "## Models" 'face 'aibo:blue-face)
                  "\n\nnull uses your Codex configuration. Changes apply on the next turn.\n\n")
          (dolist (entry `(("manager_model" . ,(concat (propertize " m " 'face 'aibo:manager-badge-face) " model"))
                           ("manager_model_reasoning_effort" . ,(concat (propertize " m " 'face 'aibo:manager-badge-face) " reasoning effort"))
                           ("bot_model" . ,(concat (propertize " b " 'face 'aibo:bot-badge-face) " model"))
                           ("bot_model_reasoning_effort" . ,(concat (propertize " b " 'face 'aibo:bot-badge-face) " reasoning effort"))
                           ("title_model" . "Title generation · model")
                           ("title_model_reasoning_effort" . "Title generation · reasoning effort")))
            (let ((key (car entry)) (label (cdr entry)))
              (when (member key '("bot_model" "title_model")) (insert "\n"))
              (insert (format "%-35s " label))
              (insert-text-button
               (or (gethash key settings) "null")
               'face 'aibo:link-face 'follow-link t
               'action
               (lambda (_)
                 (let* ((value (string-trim (read-string (concat label " (null = Codex default): ")
                                                         (or (gethash key settings) "null"))))
                        (updated (copy-hash-table settings)))
                   (puthash key (if (member value '("" "null")) :null value) updated)
                   ;; JSON nulls from the GET use nil; encode them explicitly.
                   (maphash (lambda (name model) (unless model (puthash name :null updated))) updated)
                   (aibo:api--request "PUT" "/api/settings/models" updated
                                      (lambda (_) (when (eq aibo:page 'customization) (aibo:customization)))))))
              (insert "\n")))))))))

(provide 'aibo-ui-pages)
;;; aibo-ui-pages.el ends here
