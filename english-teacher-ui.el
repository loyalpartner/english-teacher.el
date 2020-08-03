;;; -*- lexical-binding: t;no-byte-compile:t;  -*-

;;;###autoload
;; (defun english-teacher-default-format-result-function (origin translation)
;;   (format "%s\n%s" origin translation))

;;;###autoload
(defface english-teacher-backend-face
  '((t(:inherit font-lock-keyword-face)))
  "Face of Display the name of the backend.")

(defun english-teacher--backend-name ()
  " Returns the backend-name with the face"
  (propertize (symbol-name english-teacher-backend)
              'face 'english-teacher-backend-face))
;;;###autoload
(defun english-teacher-default-show-result-function (origin translation)
  (require 'posframe)
  (when (posframe-workable-p)
    (posframe-show
     " *english-teacher*"
     :string (concat (english-teacher--backend-name)
                     "\n" origin
                     "\n" translation)
     :timeout 100
     :poshandler 'posframe-poshandler-frame-bottom-center
     :min-width (frame-width)
     :internal-border-width 10)
    (unwind-protect
        (push (read-event) unread-command-events)
      (posframe-hide " *english-teacher*"))))

;;;###autoload
(defun english-teacher-eldoc-show-result-function (origin translation)
  (eldoc-message (format "%s: %s"
                         (english-teacher--backend-name)
                         translation)))

(provide 'english-teacher-ui)
