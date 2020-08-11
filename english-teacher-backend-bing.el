;;; -*- lexical-binding: t;no-byte-compile:t;  -*-

;;;###autoload
(defun english-teacher-backend-bing-request (from to text)
  (let* ((url  "https://cn.bing.com/ttranslatev3?isVertical=1&&IG=75D2DE042553494A8D8749DF4F3FA910&IID=translator.5027.3")
         (headers `(("Content-Type" . "application/x-www-form-urlencoded")))
         (data (english-teacher-format-query-string
                `(("text"        . ,text)
                  ("fromLang"     . ,from)
                  ("to"     . ,to))))
         result json)
    (setq result (english-teacher-http-post url data headers))
    (setq json (json-read-from-string result))
    (if json
        (setq result (mapconcat (lambda (x) (alist-get 'text x))
                                (alist-get 'translations (elt json 0)) ""))
      (error json))
    (cons text result)))


;;;###autoload
(cl-defmethod english-teacher-translate ((text  t) (backend (eql english-teacher-backend-bing)))
  (english-teacher-backend-bing-request "auto-detect" "zh-Hans" text))

(provide 'english-teacher-backend-bing)
