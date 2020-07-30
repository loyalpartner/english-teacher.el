;;; -*- lexical-binding: t;no-byte-compile:t;  -*-

(defun english-teacher-backend-google--build-url (from to text)
  (english-teacher-backend-google--format-url
   `(("client" . "t")
     ("ie"     . "UTF-8")
     ("oe"     . "UTF-8")
     ("sl"     . ,from)
     ("tl"     . ,to)
     ("q"      . ,text)
     ("dt"     . "bd")
     ("dt"     . "ex")
     ("dt"     . "ld")
     ("dt"     . "md")
     ("dt"     . "qc")
     ("dt"     . "rw")
     ("dt"     . "rm")
     ("dt"     . "ss")
     ("dt"     . "t")
     ("dt"     . "at")
     ("pc"     . "1")
     ("otf"    . "1")
     ("srcrom" . "1")
     ("ssel"   . "0")
     ("tsel"   . "0")
     ("tk"     . ,(english-teacher-backend-google-gen-tk text)))))

(defun english-teacher-backend-google--format-url (query-params)
  (format "%s?%s" "https://translate.google.cn/translate_a/single"
          (english-teacher-format-query-string query-params)))

;;;###autoload
(defun english-teacher-backend-google-request (from to text)
  (let* ((url (english-teacher-backend-google--build-url from to text))
         result json)
    (setq result (english-teacher-http-get url))
    (setq json (aref (json-read-from-string result) 0))
    (seq-filter (lambda (x) (elt x 0)) json)
    (setq result (mapconcat (lambda (x) (elt x 0)) json ""))
    `(,text . ,result)))

;;;###autoload
(cl-defmethod english-teacher-translate ((text  t) (backend (eql english-teacher-backend-google)))
  (english-teacher-backend-google-request "en" "zh-cn" text))

(defconst english-teacher-backend-google-bit-v-len 32)

(defun english-teacher-backend-google-gen-tk (text &optional b-d1)
  (setq b-d1 (or b-d1 (english-teacher-backend-google-get-b-d1)))
  (let* ((b (cl-first b-d1))
         (d1 (cl-second b-d1))
         (ub "+-3^+b+-f")
         (vb "+-a^+6")
         (a (cl-reduce (lambda (a e) (english-teacher-backend-google-gen-rl (+ a e) vb))
                       (encode-coding-string text 'utf-8) :initial-value b)))
    (setq a (english-teacher-backend-google-gen-rl a ub))
    (setq a (english-teacher-backend-google-logxor a d1))
    (when (< a 0) ;; (abs a) + 2^31
      (setq a (+ (english-teacher-backend-google-logand a 2147483647.0) 2147483648.0)))
    (setq a (ffloor (mod a 1e6)))
    (format "%s.%s"
            (car (split-string (number-to-string a) "\\."))
            (car (split-string (number-to-string (english-teacher-backend-google-logxor a b)) "\\.")))))

(defun english-teacher-backend-google-get-b-d1 ()
  ;; TKK='427110.1469889687'
  (list 427110 1469889687))

(defun english-teacher-backend-google-gen-rl (a b)
  (cl-loop for c from 0 below (- (length b) 2) by 3
           for d = (aref b (+ c 2)) do
           (setq d (if (>= d ?a) (- d 87) (- d ?0)))
           (setq d (if (= (aref b (1+ c)) ?+)
                       (english-teacher-backend-google-lsh a (- d))
                     (english-teacher-backend-google-lsh a d)))
           (setq a (if (= (aref b c) ?+)
                       (english-teacher-backend-google-logand (+ a d) 4294967295.0)
                     (english-teacher-backend-google-logxor a d))))
  a)

(defun english-teacher-backend-google-lsh (n d)
  "Return a floating-point number.
Shift the bits in N to the left or rihgt D places.
D is an integer."
  (let ((v (english-teacher-backend-google-number-to-bit-v n))
        (v-result (make-vector english-teacher-backend-google-bit-v-len 0)))
    (if (< d 0) ;; Shift Right Logical
        ;; [x0 x1 ... xn-d ... xn] => [0 ... 0 x0 x1 ... xn-d]
        (cl-loop for i from (abs d) below english-teacher-backend-google-bit-v-len
                 for j from 0 do
                 (aset v-result i (aref v j)))
      ;; Shift Left Logical
      ;; [x0 x1 ... xd ... xn] => [xd ... xn 0 ... 0]
      (cl-loop for i from d below english-teacher-backend-google-bit-v-len
               for j from 0 do
               (aset v-result j (aref v i))))
    (english-teacher-backend-google-bit-v-to-number v-result)))

(defun english-teacher-backend-google-number-to-bit-v (n)
  "Return a bit vector from N."
  (if (< n 0) (english-teacher-backend-google-bit-v-2comp
               (english-teacher-backend-google-number-to-bit-v (abs n)))
    (let ((v (make-vector english-teacher-backend-google-bit-v-len 0)))
      (cl-loop for i downfrom (1- english-teacher-backend-google-bit-v-len) to 0
               with q
               when (< n 1) return nil do
               (setq q (ffloor (* n 0.5)))
               (aset v i (floor (- n (* 2.0 q))))
               (setq n q))
      v)))

(defun english-teacher-backend-google-bit-v-to-number (v)
  "Return a floating-point number from V."
  (if (and (> (aref v 0) 0)
           ;; Exclude [1 0 ... 0]
           (cl-loop for i from 1 below english-teacher-backend-google-bit-v-len
                    thereis (> (aref v i) 0)))
      (- (english-teacher-backend-google-bit-v-to-number (english-teacher-backend-google-bit-v-2comp v)))
    (funcall (if (> (aref v 0) 0)  #'- #'+)
             (cl-reduce (lambda (acc e) (+ (* acc 2.0) e))
                        v :initial-value 0.0))))

(defun english-teacher-backend-google-logand (n1 n2)
  "Return a floating-point number from N1 and N2."
  (english-teacher-backend-google-logfn #'logand n1 n2))

(defun english-teacher-backend-google-logfn (fn n1 n2)
  "Helper function for logical FN."
  (let ((v1 (english-teacher-backend-google-number-to-bit-v n1))
        (v2 (english-teacher-backend-google-number-to-bit-v n2))
        (v (make-vector english-teacher-backend-google-bit-v-len 0)))
    (cl-loop for i from 0 below english-teacher-backend-google-bit-v-len do
             (aset v i (funcall fn (aref v1 i) (aref v2 i))))
    (english-teacher-backend-google-bit-v-to-number v)))

(defun english-teacher-backend-google-logxor (n1 n2)
  "Return a floating-point number from N1 and N2."
  (english-teacher-backend-google-logfn #'logxor n1 n2))

(defun english-teacher-backend-google-bit-v-2comp (v)
  "Return the two's complement of V."
  (let* ((vc (vconcat v))
         (len (length vc)))
    ;; Complement of v
    (cl-loop for i from 0 below len do
             (aset vc i (logxor (aref vc i) 1)))
    ;; vc = complement of v + 1
    (cl-loop for i downfrom (1- len) to 0
             do (aset vc i (logxor (aref vc i) 1))
             when (> (aref vc i) 0) return nil)
    vc))

(provide 'english-teacher-backend-google)
