;;; .local/straight/repos/english-teacher.el/english-teacher-backend-youdao.el -*- lexical-binding: t; -*-

(defconst english-teacher-backend-youdao-api-host "http://fanyi.youdao.com/translate_o?smartresult=dict&smartresult=rule")

(defcustom english-teacher-backend-youdao-appkey "4c6f4b914908b8af"
  "baidu appid"
  :type 'string)

(defcustom english-teacher-backend-youdao-secret-key "ZYwM1xhbq7gJmk0peuHopBucGUO9JSEG"
  "baidu secret key"
  :type 'string)


(defun english-teacher-backend-youdao--build-post-data (from to text)
  (let* ((salt (number-to-string (truncate (time-to-seconds))))
         (sign (english-teacher-backend-youdao-generate-sign text salt))
         (ts (number-to-string (truncate (* 1000 (time-to-seconds))))))
    (english-teacher-format-query-string
     `(("i"        . ,text)
       ("from"     . ,from)
       ("to"     . ,to)
       ("smartresult"     . "dict")
       ("client"     . "fanyideskweb")
       ;; ("salt"     . ,salt)
       ;; ("sign"   . ,sign)
       ;; ("ts"     . ,ts)
       ;; ("bv"       . ,(md5 "2.1"))
       ("doctype"       . "json")
       ("version"       . "2.1")
       ("keyfrom" . "fanyi.web")
       ("typoResult" . "false")
       ("action"     . "FY_BY_CLICKBUTTION")))))

(print (english-teacher-backend-youdao--build-post-data "auto" "auto" "i love you"))

(english-teacher-http-post english-teacher-backend-youdao-api-host
                         (english-teacher-backend-youdao--build-post-data "AUTO" "AUTO" "i love you"))


(defun english-teacher-backend-youdao-generate-sign (text salt)
  (let ((origin (format
                 "%s%s%s%s%s"
                 "fanyideskweb"
                 text
                 salt
                 "n%A-rKaT5fb[Gy?;N5@Tj"
                 baidu-translator-secret-key)))
    (md5 origin)))

;;;###autoload
(defun english-teacher-backend-youdao-request (from to text)
  (let* ((url (english-teacher-backend-youdao--build-url from to text))
         result json)
    (setq result (english-teacher-http-get url))
    (setq json (json-read-from-string result))
    (setq result  (alist-get 'trans_result json))
    (if result
        (setq result (mapconcat (lambda (x) (alist-get 'dst x)) result ""))
      (error (alist-get 'error_msg json)))
    
    `(,text . ,result)))

(print (english-teacher-backend-youdao--build-url "auto" "auto" "i love you"))
(english-teacher-backend-youdao-request "auto" "auto" "i love you")

;;;###autoload
(cl-defmethod english-teacher-translate ((text  t) (backend (eql english-teacher-backend-youdao)))
  (english-teacher-backend-youdao-request "auto" "auto" text))

(provide 'english-teacher-backend-youdao)
