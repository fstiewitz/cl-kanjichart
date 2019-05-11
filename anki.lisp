(ql:quickload :SPLIT-SEQUENCE)
(ql:quickload :CL-UNICODE)
(defpackage :PW.STIEWITZ.KANJICHART.ANKI
  (:use :COMMON-LISP :SPLIT-SEQUENCE :CL-UNICODE)
  (:export :load-from-kanji :learned-to-list))
(in-package :PW.STIEWITZ.KANJICHART.ANKI)
(defclass review ()
  ((time :initarg :time :accessor review-time)
   (ease :initarg :ease :accessor review-ease)
   (cnt :initarg :cnt :accessor review-cnt)))
(defvar *kanji* (make-hash-table))
(defconstant *sqlite3-query* "SELECT revlog.id, ease, sfld FROM revlog JOIN (SELECT cards.id, sfld FROM cards JOIN notes on cards.nid=notes.id) cards ON cid=cards.id")
(defun learned-to-list ()
  (let ((ret))
    (maphash (lambda (k v)
               (declare (ignore v))
               (push k ret))
             *kanji*)
    ret))
(defmacro for-each-line (sequence binding &body body)
  (let ((i-name (gensym))
        (nn-name (gensym))
        (ss-name (gensym)))
    `(do* ((,ss-name ,sequence)
           (,i-name 0 (+ ,nn-name 1))
           (,nn-name (position #\Newline ,ss-name) (position #\Newline ,ss-name :start ,i-name)))
          ((not ,nn-name))
       (let ((,binding (subseq ,ss-name ,i-name ,nn-name)))
         (progn
           ,@body)
         (if (not ,nn-name)
            (return))))))
(defconstant *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))
(defconstant *2019-start-time*
  (encode-universal-time 0 0 0 1 1 2019 0))
(defun load-from-kanji (path &key no-earlier-than)
  (multiple-value-bind (output error return-code) (uiop/run-program:run-program (list "sqlite3" path *sqlite3-query*) :output :string)
    (declare (ignore error))
    (when (= 0 return-code)
      (for-each-line output line
        (let* ((tokens (split-sequence:split-sequence #\| line))
               (time (+ (floor (parse-integer (first tokens)) 1000) *unix-epoch-difference*))
               (ease (parse-integer (second tokens)))
               (cnt (third tokens)))
          (if (and (= 1 (length cnt))
                   (string= "Han" (cl-unicode:script (elt cnt 0))))
              (if (= 1 ease)
                  (remhash (elt cnt 0) *kanji*)
                  (when (and (or (not no-earlier-than) (> time no-earlier-than))
                             (or (not (gethash (elt cnt 0) *kanji*))
                                 (> time (review-time (gethash (elt cnt 0) *kanji*)))))
                    (setf (gethash (elt cnt 0) *kanji*) (make-instance 'review :time time :ease ease :cnt cnt))))))))))
(defconstant *day-names*
  '("Monday" "Tuesday" "Wednesday"
    "Thursday" "Friday" "Saturday"
    "Sunday"))
(defun get-time (timestamp)
  (multiple-value-bind
	      (second minute hour date month year day-of-week dst-p tz)
	    (decode-universal-time timestamp)
    (declare (ignore dst-p))
    (format nil "~2,'0d:~2,'0d:~2,'0d of ~a, ~d/~2,'0d/~d (GMT~@d)"
	          hour
	          minute
	          second
	          (nth day-of-week *day-names*)
	          month
	          date
	          year
	          (- tz))))
(defun print-review (r)
  (format t "~a Ease: ~a Time: ~a~%"
          (review-cnt r)
          (review-ease r)
          (get-time (review-time r))))
(defun print-statistics ()
  (let ((ret))
    (maphash (lambda (k v)
               (declare (ignore k))
               (push v ret))
             *kanji*)
    (mapc 'print-review (sort ret '< :key 'review-time))
    nil))
