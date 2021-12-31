;;; -*- lexical-binding: t;no-byte-compile:t;  -*-

(defconst english-teacher-backend-baidu-api-host "https://api.fanyi.baidu.com/api/trans/vip/translate")

(defcustom english-teacher-backend-baidu-appid "20200607000488675"
  "baidu appid"
  :type 'string)

(defcustom english-teacher-backend-baidu-secret-key "Nb_cT61hFraVEUpkvp33"
  "baidu secret key"
  :type 'string)

(defun english-teacher-backend-baidu--build-url (from to text)
  (let* ((salt (number-to-string (random)))
         (sign (english-teacher-backend-baidu-generate-sign text salt)))
    (english-teacher-backend-baidu--format-url
     `(("q"     . ,text)
       ("salt"  . ,salt)
       ("appid" . ,english-teacher-backend-baidu-appid)
       ("from"  . ,from)
       ("to"    . ,to)
       ("sign"  . ,sign)))))

(defun english-teacher-backend-baidu--format-url (query-params)
  (format "%s?%s" english-teacher-backend-baidu-api-host
          (english-teacher-format-query-string query-params)))

(defun english-teacher-backend-baidu-generate-sign (text salt)
  (let ((origin (format
                 "%s%s%s%s"
                 english-teacher-backend-baidu-appid
                 text
                 salt
                 english-teacher-backend-baidu-secret-key)))
    (md5 origin nil nil (coding-system-from-name "utf-8"))))

;;;###autoload
(defun english-teacher-backend-baidu-request (from to text)
  (let* ((url (english-teacher-backend-baidu--build-url from to text))
         result json)
    (setq result (english-teacher-http-get url))
    (setq json (json-read-from-string result))
    (setq result  (alist-get 'trans_result json))
    (if result
        (setq result (mapconcat (lambda (x) (alist-get 'dst x)) result ""))
      (error (alist-get 'error_msg json)))
    
    `(,text . ,result)))

(cl-defmethod et-translate ((text  t)
                            (backend (eql baidu))
                            (on-translation-completed t))
  (et-send-request
   (make-translation-request
    :url (english-teacher-backend-baidu--build-url "en" "zh" text)
    :method "GET")
   (lambda (body)
     (et-baidu-on-request-completed text body on-translation-completed))))

(defun et-baidu-on-request-completed (origin-text body on-translation-completed)
  (let (json result)
    (setq json (json-read-from-string body))
    (setq result  (alist-get 'trans_result json))
    (unless result
      (error (alist-get 'error_msg json)))
    (setq result
          (mapconcat
           (lambda (x)
             (alist-get 'dst x)) result ""))
    (apply on-translation-completed `('baidu ,origin-text ,result))))

;;;###autoload
(cl-defmethod english-teacher-translate ((text  t) (backend (eql english-teacher-backend-baidu)))
  (english-teacher-backend-baidu-request "en" "zh" text))

(provide 'english-teacher-backend-baidu)
