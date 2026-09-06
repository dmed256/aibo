;;; aibo-header-test.el --- Pinned headers and event details -*- lexical-binding: t -*-
(require 'aibo-reference-test)

(ert-deftest aibo-header:account-usage ()
  (dolist (values '((75 2 "75%" "2") (0 0 "0%" "0") (nil nil "—" "—")))
    (let* ((sidebar (aibo-test:hash "account_usage"
                                    (aibo-test:hash "remaining_percent" (nth 0 values)
                                                    "resets" (nth 1 values))))
           (badges (aibo:--account-usage-badges sidebar)))
      (should (string-match-p (regexp-quote (concat " usage  " (nth 2 values) " ")) badges))
      (should (string-match-p (regexp-quote (concat " resets  " (nth 3 values) " ")) badges))))
  (let ((aibo:sidebar nil))
    (aibo:render-sidebar (aibo-test:hash "account_usage" (aibo-test:hash "remaining_percent" 75)))
    (with-current-buffer aibo:sidebar-buffer
      (should-not header-line-format)
      (should-not (string-match-p "usage" (buffer-string)))))
  (let ((aibo:sidebar (aibo-test:hash "account_usage"
                                      (aibo-test:hash "remaining_percent" 78 "resets" 3)))
        (aibo:input-target (aibo-test:chat)))
    (puthash "title" (make-string 100 ?猫) aibo:input-target)
    (dolist (width '(40 80 120))
      (let ((row (aibo:--input-status-row width)))
        (should (= (string-width row) width))
        (should (string-suffix-p " usage  78%   resets  3 " row))
        (should (string-match-p "…" row))))))

(ert-deftest aibo-header:badges ()
  (dolist (role '("bot" "manager"))
    (let* ((chat (aibo-test:hash "kind" role "label" "b2" "active" t))
           (badge (aibo:--badge chat nil t))
           (background (face-background (aibo:--badge-face chat))))
      (should (equal (substring-no-properties badge) " ● b2 "))
      (dotimes (i (length badge))
        (should (equal (aibo-reference:attribute (get-text-property i 'face badge) :background) background)))
      (should (equal (aibo-reference:attribute (get-text-property 1 'face badge) :foreground) "#8ae234"))))
  (let ((badge (aibo:--metadata-badge "tokens" "1k")))
    (should (equal (substring-no-properties badge) " tokens  1k "))
    (should (equal (aibo-reference:attribute (get-text-property 1 'face badge) :background) "#383b44"))
    (should (equal (aibo-reference:attribute (get-text-property 9 'face badge) :background) "#292c34"))))

(ert-deftest aibo-header:dark-account-badges ()
  (let ((badges (aibo:--account-usage-badges
                 (aibo-test:hash "account_usage"
                                 (aibo-test:hash "remaining_percent" 68 "resets" 3)))))
    (dolist (part '((" usage " . "#25272e") (" 68% " . "#17181d")
                    (" resets " . "#25272e") (" 3 " . "#17181d")))
      (let ((start (string-match (regexp-quote (car part)) badges)))
        (should start)
        (dotimes (offset (length (car part)))
          (should (equal (aibo-reference:attribute
                          (get-text-property (+ start offset) 'face badges) :background)
                         (cdr part))))))))

(ert-deftest aibo-header:pinned ()
  (let ((chat (aibo-test:chat)) (aibo:layout 'cowork))
    (puthash "project" (aibo-test:hash "name" (make-string 200 ?x)) chat)
    (save-window-excursion
      (aibo:render-chat chat)
      (should tab-line-format)
      (should header-line-format)
      (should-not (string-match-p "project" (buffer-string)))
      (let ((body (buffer-string)) (title (aibo:--chat-header nil))
            (metadata (aibo:--chat-header t)))
        (should (<= (string-width metadata) (1- (window-body-width))))
        (dolist (field '("project" "goal" "tokens" "elapsed"))
          (should (string-match-p field metadata)))
        (goto-char (point-max))
        (aibo:--tick-elapsed (current-buffer))
        (should (equal title (aibo:--chat-header nil)))
        (should (equal body (buffer-string)))
        (let ((lower (split-window-below)))
          (aibo:--cowork-dividers)
          (should-not (window-parameter lower 'header-line-format)))))))

(ert-deftest aibo-header:no-up-arrow ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (setq-local aibo:buffer-chat (aibo-test:chat))
      (insert (make-string 100 ?\n))
      (goto-char 20)
      (set-window-start nil (point))
      (aibo:--edge-indicator 0 (selected-window) (point) "↑" 'aibo:muted-face)
      (aibo:--page-redisplay (selected-window))
      (let ((up (cadr (assq (selected-window) aibo:edge-indicators))))
        (should-not (overlay-get up 'before-string))
        (should-not (overlay-get up 'display)))
      (should (= (window-start) 20)))))

(ert-deftest aibo-header:events ()
  (with-temp-buffer
    (aibo:--insert-message (aibo-test:chat)
                           (aibo-test:hash "kind" "event" "content" "reasoning"
                                           "data" (aibo-test:hash "item" (aibo-test:hash "type" "reasoning" "text" "Do not display this"))))
    (should (equal (buffer-string) " reasoning \n\n"))
    (erase-buffer)
    (let* ((diff "@@ -1 +1 @@\n-old\n+# literal `shell` [label](path)\n")
           (change (aibo-test:hash "path" "source file.el" "diff" diff
                                   "kind" (aibo-test:hash "type" "update" "move_path" "renamed.el"))))
      (aibo:--insert-message (aibo-test:chat)
                             (aibo-test:hash "kind" "event" "content" "fileChange"
                                             "data" (aibo-test:hash "item" (aibo-test:hash "type" "fileChange" "changes" (list change)))))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     (concat " file change \nupdate source file.el → renamed.el\n" diff "\n")))
      (goto-char (point-min)) (search-forward "source file.el")
      (should (equal (button-get (button-at (1- (point))) 'aibo-path) "source file.el"))
      (should-not (text-property-not-all (point-min) (point-max) 'display nil)))))

(ert-deftest aibo-header:hidden-return ()
  (dolist (needle '(" exec " "print" "source.el" "@@" "+new"))
    (with-temp-buffer
      (aibo:chat-mode)
      (setq aibo:buffer-chat (aibo-test:chat))
      (let* ((inhibit-read-only t)
             (command (aibo-test:hash "kind" "event" "content" "commandExecution"
                                      "data" (aibo-test:hash "item" (aibo-test:hash "type" "commandExecution" "command" "print $HOME"))))
             (file (aibo-test:hash "kind" "event" "content" "fileChange"
                                   "data" (aibo-test:hash "item" (aibo-test:hash "type" "fileChange" "changes"
                                                                                 (list (aibo-test:hash "path" "source.el" "kind" (aibo-test:hash "type" "update")
                                                                                                       "diff" "@@ -1 +1 @@\n-old\n+new\n")))))))
        (aibo:--insert-hidden (list command file)))
      (goto-char (point-min))
      (call-interactively (key-binding [return]))
      (search-forward needle)
      (backward-char)
      (call-interactively (key-binding (kbd "RET")))
      (should-not aibo:expanded-groups)
      (should (equal (buffer-substring-no-properties (point-min) (point-max)) "[2 hidden messages] ▸\n\n"))
      (should (= (point) (point-min))))))

(ert-deftest aibo-header:shell-colors ()
  (with-temp-buffer
    (let ((text "if [[ -n $HOME ]]; then\n  print -r -- \"$HOME\" # comment\nfi\n"))
      (insert text)
      (aibo:--fontify-shell (point-min) (point-max))
      (should (equal (buffer-substring-no-properties (point-min) (point-max)) text))
      (should (eq (plist-get (get-text-property 1 'face) :inherit) 'font-lock-keyword-face))
      (goto-char (point-min)) (search-forward "# comment")
      (should (eq (plist-get (get-text-property (1- (point)) 'face) :inherit) 'font-lock-comment-face))
      (should (equal (plist-get (get-text-property (1- (point)) 'face) :background) "#20232b"))
      (should (= 1 (hash-table-count aibo:shell-cache))))))

(ert-deftest aibo-header:whole-file-diff ()
  (with-temp-buffer
    (aibo:--insert-diff "one\ntwo" "add")
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "@@ -0,0 +1,2 @@\n+one\n+two\n\\ No newline at end of file\n"))
    (goto-char (point-min)) (search-forward "+one")
    (should (eq (get-text-property (1- (point)) 'face) 'aibo:diff-added-face))
    (erase-buffer)
    (aibo:--insert-diff "removed\n" "delete")
    (should (equal (buffer-substring-no-properties (point-min) (point-max)) "@@ -1,1 +0,0 @@\n-removed\n"))
    (goto-char (point-min)) (search-forward "-removed")
    (should (eq (get-text-property (1- (point)) 'face) 'aibo:diff-removed-face))))

(provide 'aibo-header-test)

(ert-deftest aibo-header:mcp ()
  (with-temp-buffer
    (aibo:--insert-message
     (aibo-test:chat)
     (aibo-test:hash "id" "call" "kind" "event" "content" "mcpToolCall"
                     "data" (aibo-test:hash
                             "item" (aibo-test:hash
                                     "type" "mcpToolCall" "server" "files" "tool" "read"
                                     "status" "failed" "arguments" (aibo-test:hash "path" "`literal`.el")
                                     "result" (aibo-test:hash
                                               "content" (list (aibo-test:hash "type" "text" "text" "# Raw output")
                                                               (aibo-test:hash "type" "image" "data" "binary-not-text"))
                                               "structuredContent" (aibo-test:hash "count" 0))
                                     "error" (aibo-test:hash "message" "File unavailable")))))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-prefix-p " mcp \nfiles/read · failed\n\narguments\n" text))
      (dolist (part '("`literal`.el" "# Raw output" "[image]" "\"count\": 0" "error: File unavailable"))
        (should (string-match-p (regexp-quote part) text)))
      (should-not (string-match-p "mcpToolCall\\|binary-not-text\\| Event " text)))
    (should-not (text-property-not-all (point-min) (point-max) 'display nil))))

(ert-deftest aibo-header:mcp-json ()
  (let* ((source "{\"ok\":false,\"value\":null,\"items\":[]}")
         (formatted (aibo:--mcp-payload source)))
    (should (string-match-p "\"ok\": false" formatted))
    (should (string-match-p "\"value\": null" formatted))
    (should (string-match-p (regexp-quote "\"items\": []") formatted))
    (should (text-property-not-all 0 (length formatted) 'face nil formatted))
    (should (equal (aibo:--mcp-payload "{incomplete") "{incomplete"))
    (with-temp-buffer
      (aibo:--insert-mcp (aibo-test:hash "server" "files" "tool" "read"
                                         "result" (aibo-test:hash
                                                   "content" (list (aibo-test:hash "type" "text" "text" source))
                                                   "structuredContent" (json-parse-string source :null-object :null :false-object :false))))
      (goto-char (point-min))
      (should (search-forward "\"ok\"" nil t))
      (should-not (search-forward "\"ok\"" nil t)))))

(ert-deftest aibo-header:expansion-during-sampling ()
  (with-temp-buffer
    (aibo:chat-mode)
    (let* ((inhibit-read-only t)
           (sample (aibo-test:hash "id" "sample-row" "kind" "event" "content" "mcpToolCall"
                                   "data" (aibo-test:hash "item" (aibo-test:hash "type" "mcpToolCall" "id" "call-1"))))
           (earlier (aibo-test:hash "id" "earlier" "kind" "system" "content" "New context")))
      (setq aibo:buffer-chat (aibo-test:chat))
      (aibo:--insert-hidden (list sample))
      (aibo:--expand-hidden (button-at (point-min)))
      ;; A snapshot may prepend a previously missing item and replace its row ID.
      (puthash "id" "stored-row" sample)
      (dotimes (_ 3)
        (erase-buffer)
        (aibo:--insert-hidden (list earlier sample))
        (should (button-get (button-at (point-min)) 'aibo-expanded))
        (should (string-match-p " mcp " (buffer-string))))
      (aibo:--expand-hidden (button-at (point-min)))
      (erase-buffer)
      (aibo:--insert-hidden (list sample))
      (should-not (button-get (button-at (point-min)) 'aibo-expanded))
      (should-not aibo:expanded-groups))))
