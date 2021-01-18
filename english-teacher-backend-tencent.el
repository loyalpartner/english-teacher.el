;;; -*- lexical-binding: t;no-byte-compile:t;  -*-

(defconst english-teacher-backend-tencent-api-host
  "https://fanyi.qq.com/api/translate")

(defvar english-teacher-backend-tencent-qtv nil "")

(defvar english-teacher-backend-tencent-qtk nil "")

(defvar english-teacher-backend-tencent-refresh-time nil "")

(defun english-teacher-backend-get-reauth-url ()
  (let ((result (english-teacher-http-get "https://fanyi.qq.com")))
    (string-match  "var reauthuri = \"\\([^\"]+\\)\";" result)
    (match-string 1 result)))

(defun english-teacher-backend-tencent-update-qtv-and-qtk ()
  (interactive)
  (let* ((reauth-url (english-teacher-backend-get-reauth-url))
         (result (english-teacher-http-post (format "https://fanyi.qq.com/api/%s" reauth-url) "qtk=&qtv="))
         json)
    (setq json (json-read-from-string result))
    (setq english-teacher-backend-tencent-qtk (alist-get 'qtk json))
    (setq english-teacher-backend-tencent-qtv (alist-get 'qtv json))
    (setq english-teacher-backend-tencent-refresh-time (time-to-seconds))))

(defun english-teacher-backend-tencent-post-data (from to text)
  (let* ((qtv english-teacher-backend-tencent-qtv)
         (qtk english-teacher-backend-tencent-qtk)
         (uuid (format "translate_uuid%d" (floor (* (time-to-seconds) 1000))))
         (refresh-time english-teacher-backend-tencent-refresh-time))

    (when (or (not refresh-time)
              (> (- (time-to-seconds) refresh-time) 20))
      (english-teacher-backend-tencent-update-qtv-and-qtk))
    (english-teacher-format-query-string
     `(("source"     . ,from)
       ("target"     . ,to)
       ("sourceText" . ,text)
       ("qtv"        . ,qtv)
       ("qtk"        . ,qtk)
       ("sessionUuid". ,uuid)))))

;;;###autoload
(defun english-teacher-backend-tencent-request (from to text)
  (let* ((url english-teacher-backend-tencent-api-host)
         (headers `(("Referer" . "https://fanyi.qq.com/")
                    ("Content-Type" .  "application/x-www-form-urlencoded; charset=UTF-8")
                    ("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OSX10_14_2) AppleWebKit/537.36(KHTML, like Gecko) Chrome/75.0.3770.100 Safari/537.36")))
         (data (english-teacher-backend-tencent-post-data from to text))
         result json)
    (setq result (english-teacher-http-post url data headers))
    (setq json (json-read-from-string result))

    (if (eq (alist-get 'errCode json) 0)
        (progn
          (setq result (alist-get 'translate json))
          (setq result (alist-get 'records result))
          (setq result
                (mapconcat (lambda (x) (alist-get 'targetText x)) result "")))
      (error (alist-get 'errMsg json)))
    (cons text result)))

;;;###autoload
(cl-defmethod english-teacher-translate ((text  t) (backend (eql english-teacher-backend-tencent)))
  (english-teacher-backend-tencent-request "auto" "zh" text))

(provide 'english-teacher-backend-tencent)
