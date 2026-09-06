;;; elisp-format.el --- Batch formatting for Aibo source -*- lexical-binding: t -*-

(require 'cl-lib)

(defconst aibo-format-root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(defun aibo-format--forms ()
  "Read forms without evaluating them, to verify formatting preserves code."
  (save-excursion
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(defun aibo-format--run (write)
  "Check native Elisp indentation, or apply it when WRITE is non-nil."
  (let (changed)
    (dolist (file (append (directory-files-recursively
                           (expand-file-name "elisp" aibo-format-root) "\\.el\\'")
                          (list (expand-file-name "scripts/elisp-format.el" aibo-format-root))))
      (with-temp-buffer
        (insert-file-contents file)
        (delay-mode-hooks (emacs-lisp-mode))
        (setq-local indent-tabs-mode nil)
        (check-parens)
        (let ((original (buffer-string))
              (forms (aibo-format--forms))
              (inhibit-message t))
          (indent-region (point-min) (point-max))
          (goto-char (point-min))
          (while (re-search-forward "[ \t]+$" nil t)
            (unless (nth 3 (save-excursion (syntax-ppss (match-beginning 0))))
              (replace-match "")))
          (unless (equal forms (aibo-format--forms))
            (error "Formatting changed Lisp forms in %s" file))
          (unless (equal original (buffer-string))
            (push (file-relative-name file aibo-format-root) changed)
            (when write (write-region (point-min) (point-max) file nil 'silent))))))
    (dolist (file (nreverse changed)) (message "%s: %s" (if write "Formatted" "Needs formatting") file))
    (when (and changed (not write))
      (error "Run emacs -Q --batch -l scripts/elisp-format.el -f aibo-format-write"))))

(defun aibo-format-check ()
  "Fail if source indentation differs from Emacs' native formatter."
  (aibo-format--run nil))

(defun aibo-format-write ()
  "Apply native indentation without evaluating project source."
  (aibo-format--run t))

;;; elisp-format.el ends here
