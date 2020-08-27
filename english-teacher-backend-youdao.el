;;; -*- lexical-binding: t;no-byte-compile:t;  -*-
(defconst english-teacher-backend-youdao-api-host
  "http://fanyi.youdao.com/translate_o?smartresult=dict&smartresult=rule")

;; https://zhuanlan.zhihu.com/p/95036714
(defvar english-teacher-backend-youdao-magic-string
  nil
  "magic string")

(defun english-teacher-backend-youdao-ts ()
  (truncate (* (time-to-seconds) 1000)) )

(defun english-teacher-backend-youdao-salt ()
  (english-teacher-backend-youdao-ts))

(defun english-teacher-backend-youdao-sign (text)
  (let ((salt (english-teacher-backend-youdao-salt))
        (ts (english-teacher-backend-youdao-ts))
        (magic-string (english-teacher-backend-youdao-get-magic-string)))
    (md5 (format "fanyideskweb%s%d%s" text salt magic-string))))

(defun english-teacher-backend-youdao-get-magic-string ()
  (or english-teacher-backend-youdao-magic-string
      (setq english-teacher-backend-youdao-magic-string
            (let ((text (english-teacher-http-get
                         "http://shared.ydstatic.com/fanyi/newweb/v1.0.28/scripts/newweb/fanyi.min.js")))
              (string-match  "\"fanyideskweb\"\\+e\\+i\\+\"\\([^\"]+\\)\"" text)
              (match-string 1 text)))))

(defun english-teacher-backend-youdao-post-data (from to text)
  (let* ((salt (number-to-string (english-teacher-backend-youdao-salt)))
         (sign (english-teacher-backend-youdao-sign text))
         (ts (number-to-string (english-teacher-backend-youdao-ts))))
    (english-teacher-format-query-string
     `(("i"        . ,text)
       ("from"     . ,from)
       ("to"     . ,to)
       ("smartresult"     . "dict")
       ("client"     . "fanyideskweb")
       ("salt"     . ,salt)
       ("sign"   . ,sign)
       ("ts"     . ,ts)
       ("bv"       . "9b36851b1c8e76d933a5d868c45f3c47")
       ("doctype"       . "json")
       ("version"       . "2.1")
       ("keyfrom" . "fanyi.web")
       ("typoResult" . "false")
       ("action"     . "FY_BY_CLICKBUTTION")))))

;;;###autoload
(defun english-teacher-backend-youdao-request (from to text)
  (let* ((url english-teacher-backend-youdao-api-host)
         (headers `(("Cookie" . "OUTFOX_SEARCH_USER_ID=1389460813@123.125.1.12")
                    ("Referer" . "http://fanyi.youdao.com/")
                    ("Content-Type" . "application/x-www-form-urlencoded")
                    ("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OSX10_14_2) AppleWebKit/537.36(KHTML, like Gecko) Chrome/75.0.3770.100 Safari/537.36")))
         (data (english-teacher-backend-youdao-post-data from to text))
         result json)
    (setq result (english-teacher-http-post url data headers))
    (setq json (json-read-from-string result))
    (setq result (alist-get 'translateResult json))
    (if result
        (setq result (mapconcat (lambda (x) (alist-get 'tgt x)) (elt result 0) ""))
      (error json))
    (cons text result)))

;;;###autoload
(cl-defmethod english-teacher-translate ((text  t) (backend (eql english-teacher-backend-youdao)))
  (english-teacher-backend-youdao-request "en" "zh-CHS" text))

(provide 'english-teacher-backend-youdao)
