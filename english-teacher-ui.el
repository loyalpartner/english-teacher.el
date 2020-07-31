;;; -*- lexical-binding: t;no-byte-compile:t;  -*-

;;;###autoload
;; (defun english-teacher-default-format-result-function (origin translation)
;;   (format "%s\n%s" origin translation))

;;;###autoload
(defun english-teacher-default-show-result-function (origin translation)
  (require 'posframe)
  (when (posframe-workable-p)
    (posframe-show
     " *english-teacher*"
     :string (concat (symbol-name english-teacher-backend) "\n" origin "\n" translation)
     :timeout 100
     :poshandler 'posframe-poshandler-frame-bottom-center
     :min-width (frame-width)
     :internal-border-width 10)
    (unwind-protect
        (push (read-event) unread-command-events)
      (posframe-hide " *english-teacher*"))))

;;;###autoload
(defun english-teacher-eldoc-show-result-function (origin translation)
  (eldoc-message (format "%s:%s"
                         (symbol-name english-teacher-backend)
                         translation)))

(provide 'english-teacher-ui)
