;;; -*- lexical-binding: t;no-byte-compile:t;  -*-

(defcustom english-teacher-backends-alist
  '((youdao . english-teacher-backend-youdao)
    (bing . english-teacher-backend-bing)
    (google . english-teacher-backend-google)
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

(define-minor-mode english-teacher-follow-mode
  "follow mode"
  :lighter ""
  :keymap (let ((map (make-sparse-keymap))) map)
  (cond (english-teacher-follow-mode (progn
                                      (setq-local sentence-end-without-space "。．？！?!;；")
                                      (add-hook 'post-command-hook #'english-teacher-follow-mode-translate nil t)))
        (t (remove-hook 'post-command-hook #'english-teacher-follow-mode-translate t))))

(defvar english-teacher-timer nil)

(defcustom english-teacher-disabled-functions nil
  ""
  :type '(list function)
  :local t)

(defun english-teacher-disabled-p ()
  ""
  (let* ((func-or-list english-teacher-disabled-functions))
    (and (cl-some #'(lambda (x)
                      (if (functionp x)
                          (funcall x)
                        nil))
                  (cond ((functionp func-or-list) (list func-or-list))
                        ((listp func-or-list) func-or-list)
                        (t nil))))))

(defmacro english-teacher-lazy-execute (func args)
  `(setq english-teacher-timer
         (run-with-idle-timer
          0.5 nil
          (lambda (args) (apply func args))
          args)))

(defun english-teacher-follow-mode-translate ()
  (when english-teacher-timer (cancel-timer english-teacher-timer))

  (require (alist-get english-teacher-backend english-teacher-backends-alist))
  (let* ((sentence (english-teacher-sentence-at-point))
         cache func args)
    (when (and sentence
               (not (english-teacher-disabled-p)))
      (setq cache (english-teacher-get-cache sentence))
      (setq func (if cache english-teacher-show-result-function #'english-teacher-translate-sentence))
      (setq args (if cache (list sentence cache) (list sentence)))
      (english-teacher-lazy-execute func args))))

(defun english-teacher-translate-sentence (sentence)
  (let* ((result (english-teacher-translate sentence (alist-get english-teacher-backend english-teacher-backends-alist)))
         (origin (car result))
         (translation (cdr result)))
    (engilsh-teacher-put-cache origin translation)
    (funcall english-teacher-show-result-function origin translation)))


(defun english-teacher-http-get (url)
  (with-current-buffer (url-retrieve-synchronously url t nil 2)
    (set-buffer-multibyte t)
    (goto-char (point-min))
    (when (not (string-match "200 OK" (buffer-string)))
      (error "Problem connecting to the server"))
    (re-search-forward "^$" nil 'move)
    (prog1
        (buffer-substring-no-properties (point) (point-max))
      (kill-buffer))))

(defun english-teacher-http-post (url data &optional headers)
  (let* ((url-request-method        "POST")
         (url-request-extra-headers headers)
         (url-request-data data))
    (with-current-buffer (url-retrieve-synchronously url t nil 2)
      (set-buffer-multibyte t)
      (goto-char (point-min))
      (when (not (string-match "200 OK" (buffer-string)))
        (error "Problem connecting to the server"))
      (re-search-forward "^$" nil 'move)
      (prog1
          (buffer-substring-no-properties (point) (point-max))
        (kill-buffer)))))

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

;;;###autoload
(cl-defgeneric english-teacher-translate (text backend)
  "return cons ceil whose car is origin and cdr is translation"
  (cons "orgin" "translation"))

(defun english-teacher-get-cache (key)
  (if-let ((cache (alist-get english-teacher-backend english-teacher-translation-cache-alist)))
      (gethash key cache)
    (add-to-list 'english-teacher-translation-cache-alist (cons english-teacher-backend (make-hash-table :test 'equal)))
    nil))

(defun engilsh-teacher-put-cache (key value)
  (puthash key value (alist-get english-teacher-backend english-teacher-translation-cache-alist)))

(defun english-teacher-choose-backend ()
  "Select the backend of the list inside"
  (interactive)
  (when-let (backend (intern (completing-read "Choose On:" (mapcar #'car english-teacher-backends-alist))))
    (setq english-teacher-backend backend)
    (english-teacher-follow-mode-translate)))

(defun english-teacher-next-backend ()
  "Select the next backend"
  (interactive)
  (let* ((backends (mapcar #'car english-teacher-backends-alist))
         (backend english-teacher-backend)
         (backend-index (seq-position backends backend)))
    (setq english-teacher-backend
          (if (< backend-index (- (length backends) 1))
              (elt backends (+ backend-index 1))
            (elt backends 0)))
    (english-teacher-follow-mode-translate)))

(defun english-teacher-in-comment-p ()
  "Test if character at POS is comment.  If POS is nil, character at `(point)' is tested"
  (let* ((pos (point))
         (fontfaces (get-text-property pos 'face)))
    (when (not (listp fontfaces))
      (setf fontfaces (list fontfaces)))
    (delq nil
          (mapcar #'(lambda (f)
                      ;; learn this trick from flyspell
                      (or (eq f 'font-lock-comment-face)
                          (eq f 'font-lock-comment-delimiter-face)))
                  fontfaces))))

(defun english-teacher-in-string-p ()
  (nth 3 (syntax-ppss)))

(defun english-teacher-string-at-point ()
  (let ((beg (point))
        (end (point)))
    (save-excursion
      (while (english-teacher-in-string-p)
        (setq beg (point))
        (backward-char)))
    (save-excursion
      (while (english-teacher-in-string-p)
        (setq end (point))
        (forward-char)))
    (buffer-substring-no-properties beg end)))

(defun english-teacher-comment-at-point ()
  (let ((beg (point))
        (end (point)))
    (save-excursion
      (while (english-teacher-in-comment-p)
        (setq beg (point))
        (backward-char)))
    (save-excursion
      (while (english-teacher-in-comment-p)
        (setq end (point))
        (forward-char)))
    (buffer-substring-no-properties beg end)))

(defun english-teacher-smart-translate (text)
  "Intelligent translation"
  (interactive (list (cond ((region-active-p) (buffer-substring-no-properties (region-beginning) (region-end)))
                           ((english-teacher-in-comment-p) (english-teacher-comment-at-point))
                           ((english-teacher-in-string-p) (english-teacher-string-at-point))
                           (t (english-teacher-sentence-at-point)))))
  (let ((english-teacher-show-result-function 'english-teacher-default-show-result-function))
    (english-teacher-translate-sentence text)
    (when (region-active-p) (deactivate-mark))))

(provide 'english-teacher-core)
