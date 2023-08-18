;;; aibo-utils.el --- Common helper utils -*- lexical-binding: t -*-

;; ---[ Logger ]----------------------------------
(defun aibo:--log (level value)
  (if aibo:enable-debug-logging
      (aibo:--get-or-create-buffer
       :name "*Aibo logs*"
       :on-load
       (lambda (buffer)
         (let* ((level-text (cond
                             ((eq level :info) "INFO")
                             ((eq level :warning) "WARNING")
                             ((eq level :error) "ERROR")))
                (level-color (cond
                              ((eq level :info) "#8cc4ff")
                              ((eq level :warning) "#e9b96e")
                              ((eq level :error) "#e57373"))))
           (with-current-buffer buffer
             (end-of-buffer)
             (insert (format
                      "%-10s %s\n"
                      (concat (propertize level-text 'face `(:foreground ,level-color)) ":")
                      value))))))))

(defun aibo:log-info (value)
  (aibo:--log :info value))

(defun aibo:log-warning (value)
  (aibo:--log :warning value))

(defun aibo:log-error (value)
  (aibo:--log :error value))

;; ---[ Misc ]------------------------------------
(defun aibo:--get-or-create-buffer (&rest args)
  "Switches to buffer 'name' and calls 'on-load' afterwards"
  (let* ((name (plist-get args :name))
         (open-style (plist-get args :open-style))
         (on-create (plist-get args :on-create))
         (on-load (plist-get args :on-load))
         (buffer (get-buffer name))
         (did-create-buffer (if buffer nil t)))

    (unless buffer
      (setq buffer (generate-new-buffer name)))

    (cond
     ((eq open-style :current-window)
      (switch-to-buffer buffer))
     ((eq open-style :new-window)
      (if (> (length (window-list)) 1)
          (switch-to-buffer buffer)
        (pop-to-buffer buffer))))

    (if (and on-create did-create-buffer)
        (with-current-buffer buffer
          (funcall on-create buffer)))

    (if on-load
        (with-current-buffer buffer
          (funcall on-load buffer)))))

(defun aibo:--get-region ()
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    nil))

(provide 'aibo-utils)
