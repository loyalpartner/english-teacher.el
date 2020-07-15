;;; .local/straight/repos/english-teacher.el/english-teacher-ui.el -*- lexical-binding: t; -*-

;;;###autoload
(defun english-teacher-default-format-result-function (origin translation)
  (format "%s\n%s" origin translation))

;;;###autoload
(defun english-teacher-default-show-result-function (result)
  (require 'posframe)
  (when (posframe-workable-p)
    (posframe-show
     "*english-teacher*"
     :string (concat (symbol-name english-teacher-backend) "\n" result)
     :timeout 100
     :poshandler 'posframe-poshandler-frame-bottom-center
     :min-width (frame-width)
     :internal-border-width 10)
    (unwind-protect
        (push (read-event) unread-command-events)
      (posframe-delete "*english-teacher*"))))

(provide 'english-teacher-ui)
