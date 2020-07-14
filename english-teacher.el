;;; .local/straight/repos/english-teacher.el/english-teacher.el -*- lexical-binding: t; -*-

(defcustom english-teacher-backends-alist
  '((google . english-teacher-backend-google)
    (baidu . english-teacher-backend-baidu))
  "backends alist"
  :type '(list))

(defcustom english-teacher-backend
  'google
  "english teacher backend"
  :type 'symbol)

(defcustom english-teacher-translation-cache-alist
  (mapcar (lambda (backend) (cons (car backend) (make-hash-table :test 'equal))) english-teacher-backends-alist)
  ""
  :type '(list))

(defcustom english-teacher-show-result-function
  'english-teacher-default-show-result-function
  ""
  :type 'function)

(defcustom english-teacher-format-result-function
  'english-teacher-default-format-result-function
  ""
  :type 'function)

(define-minor-mode english-teacher-follow-mode
  "follow mode"
  :lighter ""
  :keymap (let ((map (make-sparse-keymap))) map)
  (cond (english-teacher-follow-mode (prog
                                      (setq-local sentence-end-without-space "。．？！?!;；")
                                      (add-hook 'post-command-hook #'english-teacher-follow-mode-translate nil t)))
        (t (remove-hook 'post-command-hook #'english-teacher-follow-mode-translate t))))

(defun english-teacher-follow-mode-translate ()
  (let* ((backend english-teacher-backend)
         (provider (alist-get backend english-teacher-backends-alist)))
    (unless (featurep provider)
      (require provider)))

  (when english-teacher-timer (cancel-timer english-teacher-timer))

  (let* ((sentence (english-teacher-sentence-at-point))
         cache func args)
    (when sentence
      (setq cache (english-teacher-get-cache sentence))
      (setq func (if cache english-teacher-show-result-function #'english-teacher-translate-sentence))
      (setq args (if cache (list cache) (list sentence)))
      (english-teacher-lazy-execute func args))))

(defun english-teacher-translate-sentence (sentence)
  (let* ((result (english-teacher-translate sentence (alist-get english-teacher-backend english-teacher-backends-alist)))
         (origin (car result))
         (translation (cdr result)))
    (engilsh-teacher-put-cache origin translation)
    (funcall english-teacher-show-result-function (funcall english-teacher-format-result-function origin translation))))

;; (english-teacher-translate-sentence "hello world. i love you.")

(defun english-teacher-http-get (url)
  (with-current-buffer (url-retrieve-synchronously url t)
    (set-buffer-multibyte t)
    (goto-char (point-min))
    (when (not (string-match "200 OK" (buffer-string)))
      (error "Problem connecting to the server"))
    (re-search-forward "^$" nil 'move)
    (prog1
        (buffer-substring-no-properties (point) (point-max))
      (kill-buffer))))


(defun english-teacher-format-query-string (query-params)
  (mapconcat #'(lambda (p)
                 (format "%s=%s"
                         (url-hexify-string (car p))
                         (url-hexify-string (cdr p))))
             query-params "&"))

(cl-defgeneric english-teacher-transform-text (text mode)
  ""
  text)

(cl-defmethod english-teacher-transform-text ((text t) (mode (eql Info-mode)))
  (setq text (replace-regexp-in-string "\\*Note \\([^:]*\\)::" "See \\1" text))
  (setq text (replace-regexp-in-string "^[\\*-]{2,}" "" text)) ; remove ****** or ------------
  (setq text (replace-regexp-in-string "^\s*-- .*$" "" text)) ; -- function 
  (setq text (replace-regexp-in-string "\s+" "\n" text))
  (setq text (replace-regexp-in-string "^[=\\*-]+$" "" text)) ; ============ -----------
  (setq text (replace-regexp-in-string "CR-LF" "CRLF" text)) ;
  text)

(defun english-teacher-transform-special-text (text)
  (when text
    (setq text (english-teacher-transform-text text major-mode))
    (setq text (string-trim (replace-regexp-in-string "\s*\n+\s*" " " text)))
    text))

(defun english-teacher-sentence-at-point ()
  (english-teacher-transform-special-text (thing-at-point 'sentence t)))

(defun english-teacher-paragraph-at-point ()
  (english-teacher-transform-special-text (thing-at-point 'paragraph t)))

(cl-defgeneric english-teacher-translate (text backend)
  "return cons ceil whose car is origin and cdr is translation"
  (cons "orgin" "translation"))

(defun english-teacher-get-cache (key)
  (gethash key (alist-get english-teacher-backend english-teacher-translation-cache-alist)))

(defun engilsh-teacher-put-cache (key value)
  (puthash key value (alist-get english-teacher-backend english-teacher-translation-cache-alist)))

(defvar english-teacher-timer nil)

(defmacro english-teacher-lazy-execute (func args)
  `(setq english-teacher-timer
         (run-with-idle-timer
          0.5 nil
          (lambda (args) (apply func args))
          args)))

(defun english-teacher-default-format-result-function (origin translation)
  (format "%s\n%s" origin translation))

(defun english-teacher-default-show-result-function (result)
  (require 'posframe)
  (when (posframe-workable-p)
    (posframe-show
     "*english-teacher*"
     :string result
     :timeout 100
     :poshandler 'posframe-poshandler-frame-bottom-center
     :min-width (frame-width)
     :internal-border-width 10)
    (unwind-protect
        (push (read-event) unread-command-events)
      (posframe-delete "*english-teacher*"))))

(provide 'english-teacher)

