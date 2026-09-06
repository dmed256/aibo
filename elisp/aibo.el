;;; aibo.el --- Start and use Aibo -*- lexical-binding: t -*-
(eval-and-compile
  (let ((load-prefer-newer t))
    (require 'aibo-api)
    (require 'aibo-ui)))
(declare-function websocket-openp "websocket")

(defcustom aibo:python-command "python"
  "Python interpreter with the Aibo package installed."
  :type 'string :group 'aibo)
(defcustom aibo:auto-start t
  "Start the local server and bouncer when opening an unavailable workspace."
  :type 'boolean :group 'aibo)
(defvar aibo:startup-process nil)

(defun aibo:initialize ()
  "Open Aibo, starting its local services when necessary."
  (interactive)
  (cond ((or (not aibo:auto-start)
             (and aibo:api--websocket (websocket-openp aibo:api--websocket)))
         (aibo:homepage))
        ((process-live-p aibo:startup-process)
         (message "Aibo is starting; details in *Aibo startup*"))
        (t
         (setq aibo:startup-process
               (make-process
                :name "aibo-startup" :buffer "*Aibo startup*" :noquery t
                :command (list aibo:python-command "-m" "aibo.cli.deploy" "ensure" "--url" aibo:server-url)
                :sentinel (lambda (process _event)
                            (when (memq (process-status process) '(exit signal))
                              (setq aibo:startup-process nil)
                              (if (= (process-exit-status process) 0)
                                  (aibo:homepage)
                                (message "Aibo startup failed; see *Aibo startup*")))))))))

(global-set-key (kbd "C-M-h") #'aibo:initialize)
(dolist (map (list aibo:chat-mode-map aibo:input-mode-map))
  (define-key map (kbd "C-M-h") #'aibo:initialize))
(provide 'aibo)
