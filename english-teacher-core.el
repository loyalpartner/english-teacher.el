;;; -*- lexical-binding: t;no-byte-compile:t;  -*-

(defcustom english-teacher-backends-alist
  '((youdao  . english-teacher-backend-youdao)
    (tencent . english-teacher-backend-tencent)
    (bing    . english-teacher-backend-bing)
    (google  . english-teacher-backend-google)
    (baidu   . english-teacher-backend-baidu))
  "backends alist"
  :type '(list))

(defcustom english-teacher-backend
  'google
  "english teacher backend"
  :type 'symbol)

(defcustom english-teacher-translation-cache-alist
  (mapcar (lambda (backend)
            (cons
             (car backend)
             (make-hash-table :test 'equal)))
          english-teacher-backends-alist)
  ""
  :type '(list))

(defcustom english-teacher-show-result-function
  'english-teacher-default-show-result-function
  ""
  :type 'function)

(defvar english-teacher-timer nil "")

(defun english-teacher-debounce (func wait &rest options)
  (let ((timer english-teacher-timer))
    (lambda ()
      (when timer
        (cancel-timer timer)
        (setq timer nil))
      (setq timer
            (run-with-idle-timer
             wait nil
             (lambda () (apply func options)))))))

(defvar english-teacher-post-command-hooker
  (english-teacher-debounce #'english-teacher-follow-mode-translate 0.5) "")

(define-minor-mode english-teacher-follow-mode
  "follow mode"
  :lighter " etf"
  :keymap (let ((map (make-sparse-keymap))) map)
  (cond (english-teacher-follow-mode
         (progn
           (add-hook 'post-command-hook english-teacher-post-command-hooker nil t)))
        (t (progn (remove-hook 'post-command-hook english-teacher-post-command-hooker t)
                  (when english-teacher-timer (cancel-timer english-teacher-timer))))))

(defvar english-teacher-timer nil)

(defcustom english-teacher-disabled-functions nil
  ""
  :type '(list function))

(defcustom english-teacher-get-text-function
  'english-teacher-sentence-at-point
  ""
  :type 'function)

(defun english-teacher-disabled-p ()
  ""
  (let* ((disabled-functions
          (if (functionp english-teacher-disabled-functions)
              (list english-teacher-disabled-functions)
            english-teacher-disabled-functions)))
    (seq-some #'funcall disabled-functions)))

(defmacro english-teacher-lazy-execute (func args)
  `(setq english-teacher-timer
         (run-with-idle-timer
          0.5 nil
          (lambda (args) (apply func args))
          args)))


(defun english-teacher-follow-mode-translate ()
  (require (alist-get english-teacher-backend english-teacher-backends-alist))
  (let* ((text (funcall english-teacher-get-text-function))
         cache func args)
    (when (and text (not (english-teacher-disabled-p)))
      (setq cache (english-teacher-get-cache text))
      (setq func (if cache english-teacher-show-result-function #'english-teacher-translate-sentence))
      (setq args (if cache (list text cache) (list text)))

      (apply func args))))

(defun english-teacher-translate-sentence (sentence)
  (let* ((backend (alist-get english-teacher-backend english-teacher-backends-alist))
         (result (english-teacher-translate sentence backend))
         (origin (car result))
         (translation (cdr result)))
    (unless (string-empty-p translation)
      (engilsh-teacher-put-cache origin translation))
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
         (backend english-teacher-backend))
    (setq english-teacher-backend
          (or (second (member backend backends))
              (first backends)))
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


(defun et-send-request(request on-request-completed)
  (let* ((url-request-method (translation-request-method request))
         (url-request-extra-headers (translation-request-headers request))
         (url-request-data (translation-request-payload request)))
    (url-retrieve
     (translation-request-url request)
     'et--read-body
     `(,on-request-completed)
     'silent)))

(defun et--read-body (status on-request-completed)
  (with-current-buffer (current-buffer)
    (set-buffer-multibyte t)
    (goto-char (point-min))
    (unless (string-match "200 OK" (buffer-string))
      (error "Problem connecting to the server"))
    (re-search-forward "^$" nil 'move)
    (let ((message (buffer-substring-no-properties (point) (point-max))))
      (apply on-request-completed `(,message))
      (kill-buffer))))

(defvar et-translations-alist
  ;; (mapcar (lambda (backend)
  ;;           (cons
  ;;            (car backend)
  ;;            (make-hash-table :test 'equal)))
  ;;         english-teacher-backends-alist)
  '(baidu  ,(make-hash-table :test 'equal))
  ""
  :type '(list))

(cl-defstruct (translation-specifics (:constructor make-translation-specifics))
  (source "zh")
  (target "en")
  (engine "baidu")
  (completed nil)
  (origin-text nil)
  (translation nil))

(cl-defstruct (translation-request(:constructor make-translation-request))
  url
  method
  payload
  headers)

;;;###autoload
(cl-defgeneric et-translate (text backend &optional on-translation-completed)
  (cons "orgin" "translation"))

(defun et-translate-text (text)
  (et-translate text 'baidu 'et--on-translation-completed))

(et-translate-text "hello world!")
(defun et--on-translation-completed (backend origin-text translation)
  (let ((specific-translations (alist-get backend et-translations-alist)))
    (puthash origin-text translation specific-translations)
    (apply english-teacher-show-result-function `(,origin-text  ,translation))))



(provide 'english-teacher-core)
