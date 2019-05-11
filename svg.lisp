(load "anki.lisp")
(load "kanjidic.lisp")
(defpackage :PW.STIEWITZ.KANJICHART.SVG
  (:use :COMMON-LISP :PW.STIEWITZ.KANJICHART.ANKI :PW.STIEWITZ.KANJICHART.KANJIDIC)
  (:export :main :make-kanji-chart))
(in-package :PW.STIEWITZ.KANJICHART.SVG)
(defun get-learned-kanji ()
  (pw.stiewitz.kanjichart.kanjidic:intersect (pw.stiewitz.kanjichart.anki:learned-to-list)))
(defun tag-start (name &optional attrs)
  (format nil "<~a~:{ ~a=\"~a\"~}>" (remove #\< (string-downcase (string name))) attrs))
(defun tag-end (name)
  (format nil "</~a>" (remove #\< (string-downcase (string name)))))
(defmacro tag ((name &optional attrs) &body body)
  `(progn (tag-start (quote ,name) ,attrs)
          ,@body
          (tag-end (quote ,name))))
(defun mk-tag (name)
  (lambda (&rest args)
    (let* ((body)
           (args (loop for arg in args
                       when (and (consp arg) (or (stringp (car arg))
                                                 (symbolp (car arg)))
                                 (not (consp (cdr arg))))
                         collect (list (string-downcase (string (car arg))) (cdr arg))
                         else do (push arg body))))
      (append (list (tag-start name args))
              (flatten body)
              (list (tag-end name))
              (list (string #\Newline))))))
(defmacro attr (arg val)
  `(cons (quote ,arg) ,val))
(defmacro xml (stream root)
  `(handler-bind ((undefined-function (lambda (c)
                                        (use-value (mk-tag (cell-error-name c))))))
     (format ,stream "~{~a~}" ,root)))
(defun flatten (l)
  (cond ((null l) nil)
        ((or (atom l) (and (consp l)
                           (or (stringp (car l))
                               (symbolp (car l)))
                           (or (stringp (cdr l))
                               (numberp (cdr l)))))
         (list l))
        (t (loop for a in l appending (flatten a)))))
(defmacro svg (path &body body)
  `(with-open-file (stream ,path :direction :output :if-exists :supersede)
     (xml stream
       (<svg
        (attr "xmlns" "http://www.w3.org/2000/svg")
        (attr version "1.1")
        (attr baseProfile "full")
        ,@body))))
(defun make-kanji-state-list ()
  (loop for kanji in pw.stiewitz.kanjichart.kanjidic:*kanji*
        with learned = (get-learned-kanji)
        collect (list kanji (if (member kanji learned) t nil))))
(defparameter *kanji-style*
  (concatenate 'string
               "font-style:normal;"
               "font-variant:normal;"
               "font-weight:normal;"
               "font-stretch:normal"
               "line-height:1.25;"
               "font-family:'VL Gothic';"
               "text-align:start;"
               "letter-spacing:0px;"
               "word-spacing:0px;"
               "text-anchor:start;"
               "fill-opacity:1;"
               "font-size:50px;"))
(defun kanji-to-grid (kanji*)
  (let* ((width 0)
         (skip 60)
         (skipx 30)
         (skipy 30)
         (height (* 7 11 skip))
         (elements (loop for kanji in kanji*
                         with x = skip
                         with y = skip
                         with maxy = (* 10 skip)
                         with maxx = (* 10 skip)
                         with group-offset-x = 0
                         with group-offset-y = 0
                         with group-max-y = (* 7 10 skip)
                         collect (let ((element `(<text
                                                  (attr x ,(+ x group-offset-x))
                                                  (attr y ,(+ group-offset-y y))
                                                  (attr style *kanji-style*)
                                                  (attr fill ,(if (second kanji)
                                                                  (case (kanji-grade (first kanji))
                                                                    (1 "#ff0000")
                                                                    (2 "#ff8000")
                                                                    (3 "#80c000")
                                                                    (4 "#00a000")
                                                                    (5 "#00a0ff")
                                                                    (6 "#0000ff"))
                                                                  "grey"))
                                                  ,(string (kanji-literal (car kanji))))))
                                   (setf y (+ y skip))
                                   (when (> y maxy)
                                     (setf x (+ x skip))
                                     (when (> x maxx)
                                       (setf group-offset-y (+ group-offset-y maxy skipy))
                                       (when (> group-offset-y group-max-y)
                                         (setf group-offset-y 0)
                                         (setf group-offset-x (+ group-offset-x maxx skipx))
                                         (setf width (+ group-offset-x maxx skipx)))
                                       (setf x skip))
                                     (setf y skip))
                                   element))))
    (append (list width height) (mapcar (lambda (element)
                                          (loop for item in element
                                                collect (if (and (listp item)
                                                                 (eq 'attr (car item))
                                                                 (eq 'x (cadr item))
                                                                 (numberp (caddr item)))
                                                            `(attr x ,(- width (caddr item)))
                                                            item)))
                                        elements))))
(defun make-kanji-chart (&optional (path "chart.svg"))
  (handler-bind ((undefined-function (lambda (c) (use-value (mk-tag (cell-error-name c))))))
    (let* ((kanji* (make-kanji-state-list))
           (elements (flatten (mapcar (lambda (c) (if (and (consp c) (not (consp (cdr c))))
                                                      c
                                                      (eval c)))
                                      (kanji-to-grid kanji*)))))
      (svg path
        (attr width (first elements))
        (attr height (second elements))
        (cddr elements)))))
(defun main (args)
  (if (not (= 4 (length args)))
      (format t "Arguments: kanjidic2.xml collection.anki2 output.svg~%")
      (let ((kanjidic2.xml (second args))
            (anki2 (third args))
            (output (fourth args)))
        (pw.stiewitz.kanjichart.kanjidic:read-kanjidic2.xml kanjidic2.xml)
        (pw.stiewitz.kanjichart.anki:load-from-kanji anki2)
        (make-kanji-chart output))))
